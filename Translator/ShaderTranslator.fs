namespace SharpShaders

open System
open System.Collections.Generic
open System.Reflection
open System.Text
open SharpDX
open SharpDX.Direct3D11
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open System.Diagnostics
open System.Runtime.InteropServices
open SharpShaders.Math

type ShaderEntry = ReflectedDefinitionAttribute
type ShaderFunction = ReflectedDefinitionAttribute

//=================================================================================================
module Semantics =
    let private inputSemantics = dict ["Position", "POSITION";
                                       "PositionHS", "SV_POSITION"
                                       "PositionWS", "TEXCOORD"
                                       "UV", "TEXCOORD"
                                       "Normal", "NORMAL"]


    let map fields =
        let gather (semantics,i) fieldName =
            match inputSemantics.TryGetValue(fieldName) with
            | false, _ //-> ""::semantics, i
            | true, "TEXCOORD" -> (sprintf "TEXCOORD%d" i)::semantics, i+1
            | true, s -> s::semantics, i
        let semantics, i = fields |> List.fold gather ([],0)
        semantics |> List.rev

//=================================================================================================
module InputFormats =
    let private inputFormats = dict ["UV", DXGI.Format.R32G32_Float]
    let map name = 
        if inputFormats.ContainsKey(name) then
            inputFormats.[name]
        else
            DXGI.Format.R32G32B32_Float

//=================================================================================================
module InputElements =
    /// By sticking to some conventions we can determine the shader input elements
    /// from the vertex type.
    /// The input elements offsets are calculated by matching the names in the vertex
    /// buffer element type, and calculating the offset by reflection.
    /// This allows us to use different vertex shaders at runtime on geometry which vertex 
    /// buffer layout is determined at import time.
    /// Note that this implies that vertex shader input element names have to match the
    /// names in the vertex <see cref=" Vertex "> vertex </see> elements.
    let map(vertexType:Type) (vertexShaderInputType:Type) =
        let vertexProperties = vertexType.GetProperties()
        let shaderProperties = vertexShaderInputType.GetProperties()
        let toInputElement i (semantic, name) =
            /// Semantic names like TEXCOORD cannot have a numeric suffix when used
            /// with an InputElement. Instead, the suffix is stored as the semantic index.
            let semanticName, semanticIndex = 
                let numbers, letters = semantic
                                       |> Seq.toList
                                       |> List.partition Char.IsDigit
                String(letters |> List.toArray),
                match numbers with
                                    | [x:_] -> int(Char.GetNumericValue(x))
                                    | _ -> 0
            let alignedByteOffset =
                let offset o (p:PropertyInfo) =
                    o + Marshal.SizeOf(p.PropertyType)
                vertexProperties
                |> Seq.takeWhile(fun p -> not(p.Name = name))
                |> Seq.fold offset 0                         
            InputElement(SemanticName = semanticName,
                         SemanticIndex = semanticIndex,
                         Format = InputFormats.map name,
                         AlignedByteOffset = alignedByteOffset,
                         Classification = InputClassification.PerVertexData)
        let propertyNames = shaderProperties
                            |> Array.toList
                            |> List.map (fun p -> p.Name)
        let semantics = Semantics.map propertyNames 
        propertyNames
        |> List.zip semantics
        |> Seq.mapi toInputElement


/// We need a dummy type that represents a texture. We could use a DirectX Texture2D object 
/// but is expensive to instantiate and this class will only be used for unit testing.
type Texture =
    abstract member Sample : SamplerStateDescription*float2 -> float4
    abstract member Sample : SamplerStateDescription*float32 -> float4

type CpuTexture1D(texture:float4[]) =
    let length = Array.length texture 
    interface Texture with
        member m.Sample(sampler:SamplerStateDescription, pos:float2) = 
            failwith "2D index into 1D texture"
            float4.zero
        member m.Sample(sampler:SamplerStateDescription, pos:float32) = 
            texture.[int(pos*float32(length)) % length]

type CpuTexture2D(texture:float4[,]) =
    let length1 = Array2D.length1 texture 
    let length2 = Array2D.length2 texture 
    let sample(pos:float2) =
        let sample x length = int(x*float32(length)) % length
        texture.[sample pos.x length1, sample pos.y length2]
    interface Texture with
        member m.Sample(sampler:SamplerStateDescription, pos:float2) = 
            sample pos
        member m.Sample(sampler:SamplerStateDescription, pos:float32) = 
            sample(float2(pos, 0.0f))

/// In order to write shader code in F# and execute the instructions on the GPU,
/// we need an F# to HLSL translator. F# quotations allows us to easily obtain the
/// abstract syntax tree for any given F# code. This class is responsible for generating
/// equivalent HLSL. There are some conventions and restrictions that need to be adhered to:
/// A shader in F# is represented by a class that takes its constant paramters as the only
/// argument to its constructor. This has to be a struct since this data will eventually 
/// have to be serialized and sent to the GPU.
/// An F# shader should have exactly one public method which is the entry point to the shader.
/// This method has to be flagged with the 'ReflectedDefinition' attribute. This attribute 
/// allows us to make use of quotations to get the abstract syntax tree. At the same time,
/// we are still able to test our shader code by executing the F# code on the CPU.
/// The HLSL input and output structs for the shader is generated from the input paramters
/// and return type of this shader method. Semantics are inferred from the field names:
/// Position -> POSITION
/// PositionHS -> SV_POSITION
/// Allowed types are: Color, float4x4, float4 
module ShaderTranslator =
    let private typeMapping = dict ["float4x4", "row_major matrix"
                                    "float3x3", "row_major float3x3"
                                    "Color4", "float4"
                                    "Single", "float"
                                    "Int32", "int"]
    let private methodMapping = dict ["saturatef", "saturate"
                                      "lerpf", "lerp"
                                      "Abs", "abs"
                                      "Sin", "sin"]
    let private valueOrKey(d:IDictionary<string, string>) key =
        if d.ContainsKey(key) then
            d.[key]
        else
            key
        
    let private mapMethod = valueOrKey methodMapping
    let private mapType = valueOrKey typeMapping

    /// The field members of the HLSL struct are constructed from the reflected
    /// properties of the F# type.
    let private fieldMembers(t:Type) =
        let properties = 
            let metaDataToken(pi:PropertyInfo) = pi.MetadataToken
            t.GetProperties() 
            |> Array.toList
            |> List.sortBy metaDataToken
            
        let field(semantic, p:PropertyInfo) = 
            let hlslType = mapType p.PropertyType.Name
            /// We use semantics only when the variable name matches
            /// one of the predefined names which are chosen by convention
            /// For example: PositionHS refers to a position in homogeneous space
            /// and will have the SV_POSITION semantic
            /// TEXCOORD are special since they can be in the input variables
            /// can be in any order. We append an incrementing number at the end
            /// for fields with this semantic
            let formatField = sprintf "    %s %s%s;\n" hlslType p.Name
            match semantic with
            | "" -> formatField ""
            | _ -> formatField (sprintf " : %s" semantic)
                
        let semantics = properties
                        |> List.map (fun p -> p.Name)
                        |> Semantics.map 
        properties
        |> List.zip semantics
        |> List.map field
        |> String.concat "\n"

    let private constructorParams(shaderType:Type) =
        /// We assume the first constructor is the one and only constructor
        let ctor = Seq.head(shaderType.GetConstructors())
        ctor.GetParameters()

    /// The constructor of the shader type will contain all the constant types needed by the shader
    let constantTypes(shaderType:Type) =
        constructorParams(shaderType)
        |> Seq.map (fun p -> p.ParameterType)
        |> Seq.filter (fun t -> not(t = typeof<Texture>))
        |> Seq.filter (fun t -> not(t = typeof<SamplerStateDescription>))
    
    /// The constant buffers are generated from the types of the constructor parameters
    /// of the F# shader type. This could include scene constants, object constants, material constants, etc.
    let constants(shaderType:Type) =
        let constantBuffer(constantsType:Type) =
            let formatTypeDecl = sprintf @"
cbuffer %s
{
%s
};"
            formatTypeDecl constantsType.Name (fieldMembers constantsType)
        constantTypes shaderType
        |> Seq.map constantBuffer

    /// We need to declare all the textures use by the shader and assign them to registers.
    let textures(shaderType:Type) =
        let texture i (p:ParameterInfo) = sprintf "Texture2D %s : register(t%d);" p.Name i
        constructorParams(shaderType)
        |> Seq.filter (fun p -> p.ParameterType = typeof<Texture>)
        |> Seq.mapi texture

    /// We need to declare all the samplers that will be used by this shader and assign
    /// registers for each
    let samplers(shaderType:Type) =
        let sampler i (p:ParameterInfo) = sprintf "SamplerState %s : register(s%d);" p.Name i
        constructorParams(shaderType)
        |> Seq.filter (fun p -> p.ParameterType = typeof<SamplerStateDescription>)
        |> Seq.mapi sampler

    /// Generate HLSL for F# type
    let formatStruct(t:Type) = 
        if t = typeof<float4> then
            ""
        else
            let formatInput = sprintf @"
struct %s
{
%s
};"
            formatInput t.Name (fieldMembers t)

    let private methods(t:Type) =
        let ``first private then public in declaration order``(mi:MethodInfo) = 
            let token = mi.MetadataToken
            
            if mi.IsPublic then token + 100 else token
        t.GetMethods(
                BindingFlags.Static |||
                BindingFlags.DeclaredOnly |||
                BindingFlags.Instance |||
                BindingFlags.Public   |||
                BindingFlags.NonPublic)
        // The order in which fields are returned are dependent on some internal .NET reflection cache
        // We sort by the metadata token to get the order in which the methods were declared
        |> Seq.sortBy ``first private then public in declaration order``
        

    /// We need to find all the input parameters to all the methods in the shader class.
    /// We do not need to look at output parameters since any output parameter is also
    /// the input of the next shader in the pipeline. 
    let inputStructs(shaderType:Type) =
        let parameters(mi:MethodInfo) = mi.GetParameters()
        let parameterType(pi:ParameterInfo) = pi.ParameterType
        methods shaderType
        //Only create input structs for entry points.
        |> Seq.filter (fun mi -> mi.Name = "pixel" || mi.Name = "vertex")
        |> Seq.collect parameters
        |> Seq.map parameterType
        |> Seq.map formatStruct

    // Renames variables so that inner scope variables will always have a different name than the outer scope
    let rec private renameVars depth vars  = function
    | Let(v, e1,e2) -> 
        let name =
            sprintf "%s%s" (String.init depth (fun i -> "_") ) v.Name

        let v2 = Var(name, v.Type)
        let newVars = (Map.add v v2 vars)
        Expr.Let(v2, renameVars (depth+1) newVars e1, renameVars depth newVars e2)
        //Expr.Let(v, e1, e2)
    | ExprShape.ShapeVar v when Map.containsKey v vars -> Expr.Var(vars.[v])
    | ExprShape.ShapeVar v -> Expr.Var v
    | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v,renameVars depth vars expr)
    | ExprShape.ShapeCombination(o, exprs) ->
        ExprShape.RebuildShapeCombination(o, List.map (renameVars depth vars) exprs)
    
    let rec private invertPipelines = function
    | SpecificCall(<@ (|>) @>) (_, _, exprs)->
        match exprs with
        |[e;Lambda(var, Call(e2,mi, e3))] ->
            /// Replace the last element in the list with the first expression 
            let args = e::(e3 |> List.rev |> List.tail) |> List.rev
            invertPipelines(Expr.Call(mi, args))
        |[e1;Let(_,e2,Lambda(_,Call(_,mi, e3)))] -> 
            invertPipelines(Expr.Call(mi, [e2; e1]))
        | _ -> failwith "unexpected use of pipelining operator"
    | ExprShape.ShapeVar v -> Expr.Var v
    | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v, invertPipelines expr)
    | ExprShape.ShapeCombination(o, exprs) ->
        ExprShape.RebuildShapeCombination(o, List.map invertPipelines exprs)

    let rec firstLet = function
    | Let(v, e1,e2) as l -> [l]
    | ExprShape.ShapeVar v -> []
    | ExprShape.ShapeLambda(v, expr) -> firstLet expr
    | ExprShape.ShapeCombination(o, exprs) ->
        List.map firstLet exprs
        |> List.concat
    let rec removeLet = function
    | Let(v, e1,e2) -> e2
    | ExprShape.ShapeVar v -> Expr.Var v
    | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v, removeLet expr)
    | ExprShape.ShapeCombination(o, exprs) ->
        ExprShape.RebuildShapeCombination(o, List.map removeLet exprs)
    // Renames variables so that inner scope variables will always have a different name than the outer scope
    let rec private orderLetBindings expr =
        let order parameters args rebuild = 
            let temp = function
                | Let(v,e1,e2), (x:ParameterInfo) ->
                    let name = sprintf "temp_%s" x.Name
                    Expr.Let(Var(name, x.ParameterType),
                             Expr.Let(v,e1,e2), 
                             expr)
                | e,arg -> e
            let ps = parameters
                     |> Array.toList
                     |> List.zip args
                     |> List.map temp
            let replaceLet = function
            | Let(v,_,_) -> Expr.Var v
            | e -> e
                
            let rec extractLet inner = function
            | [] -> inner
            | Let(v,e1,e2)::xs -> Expr.Let(v, e1, extractLet inner xs)
            | x::xs -> extractLet inner xs
            let reduced = rebuild(List.map replaceLet ps)
            extractLet reduced ps

        match expr with
        | Call(None, mi, args)-> 
            let rebuild args = Expr.Call(mi, args)
            let parameters = mi.GetParameters()
            order parameters args rebuild
        | NewObject(ci, args)->
            let rebuild args = Expr.NewObject(ci, args)
            let parameters = ci.GetParameters()
            order parameters args rebuild

        | ExprShape.ShapeVar v -> Expr.Var v
        | ExprShape.ShapeLambda(v, expr) -> 
            Expr.Lambda(v,orderLetBindings expr)
        | ExprShape.ShapeCombination(o, exprs) ->
            (*let reduceLet = function
            | Let(_,_,e2) -> e2
            | e -> e
            let orderedExpr = exprs 
                              |> List.map orderLetBindings
            let reduced = orderedExpr 
                          |> List.map reduceLet
            let rec insertLets outer exprs = 
                match exprs with 
                | [] -> outer
                | Let(v,e1,e2)::xs ->
                    match outer with
                    | Let(o,o1,o2) ->
                        let inner = Expr.Let(v, e1, o1)
                        insertLets (Expr.Let(o,inner, o2)) xs
                    | _ ->
                        insertLets (Expr.Let(v, e1, orderLetBindings outer)) xs
                | x::xs -> 
                    insertLets outer xs

            let outer = ExprShape.RebuildShapeCombination(o, reduced)
            let ret = insertLets outer orderedExpr
            ret*)
            ExprShape.RebuildShapeCombination(o, List.map orderLetBindings exprs)
    // Create temporary values for external method calls since they
    // cannot easily be inlined inside expressions. The method call is moved to
    // just after the surrounding let binding and the application is assigned to 
    // a temporary variable. This variable is then used in the expression instead
    // of inlining the external method
    let private inlineExternalMethodCalls expr =
        let rec next = function
        | Let(v, e1,e2) as l ->
            let xs, expr = replaceExternalMethodCalls [] e1
            let rec insertTemp cont = function
            | [] -> cont
            | (v,expr)::xs -> Expr.Let(v, expr, insertTemp cont xs)
            Expr.Let(v, insertTemp expr xs, next e2)
        | Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) as e ->
            let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
            let res = Expr.Applications(this, [ for a in args -> [a]])
            next res
        | ExprShape.ShapeVar v -> Expr.Var v
        | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v,next expr)
        | ExprShape.ShapeCombination(o, exprs) ->
            ExprShape.RebuildShapeCombination(o, List.map (next) exprs)

        and replaceExternalMethodCalls xs = function
        | Let(v, e1,e2) as l -> xs, next l
        | Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) as e ->
            let v = Var(sprintf "temp%A" (List.length xs), e.Type)
            (v,e)::xs, Expr.Var(v)
        | ExprShape.ShapeVar v -> xs, Expr.Var v
        | ExprShape.ShapeLambda(v, expr) -> 
            let xs, expr = replaceExternalMethodCalls xs expr
            xs, Expr.Lambda(v, expr)
        | ExprShape.ShapeCombination(o, exprs) ->
            let children, exprs = List.map (replaceExternalMethodCalls []) exprs
                                  |> List.unzip
            List.append xs (List.concat children),
            ExprShape.RebuildShapeCombination(o, exprs)

        next expr
    // Expand function will recursively extract all inner functions
    let rec private expand vars expr = 
        let (|IndirectProperty|_|) = function
            /// Handle multiple levels of indirection such as input.Position.xyz
            /// F# creates temporary objects which are not needed in HLSL 
            | Let(var, e1, e2) ->
                match var.Name, e1, e2 with
                | 
                    "copyOfStruct", 
                    PropertyGet(Some(x),pi, _),
                    PropertyGet(_,pi2, _) ->
                    Some(Expr.PropertyGet(e1, pi2, []))
                | _ -> None
            | _ -> None
        let expanded = 
            // First recursively process & replace variables
            match expr with
            // If the variable has an assignment, then replace it with the expression
            | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]
            // Apply 'expand' recursively on all sub-expressions
            | ExprShape.ShapeVar v -> Expr.Var v
            // Replace field get expressions with Var since the only fields
            // are local scene constants which are global in HLSL
            // For example mat.DiffuseColor maps to the DiffuseColor constant
            | PropertyGet(Some(FieldGet(_, fi)), pi, _) ->
                Expr.Var(Var(pi.Name, pi.PropertyType))
            | IndirectProperty(e) -> e
            | Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) as e ->
                let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
                let res = Expr.Applications(this, [ for a in args -> [a]])
                renameVars 0 Map.empty (expand vars res)
            | ExprShape.ShapeLambda(v, expr) ->
                Expr.Lambda(v, expand vars expr)
            | ExprShape.ShapeCombination(o, exprs) ->
                ExprShape.RebuildShapeCombination(o, List.map (expand vars) exprs)
    
        // After expanding, try reducing the expression - we can replace 'let'
        // expressions and applications where the first argument is lambda
        match expanded with
        | Application(ExprShape.ShapeLambda(v, body), assign) ->
            expand (Map.add v (expand vars assign) vars) body
        | Let(v, Lambda(input,expr), body) ->
            let assign = Expr.Lambda(input, expr)
            expand (Map.add v (expand vars assign) vars) body
//        | Let(v, assign, body) ->
//            expand (Map.add v (expand vars assign) vars) body
        | Call(None,opComposeRight,[Lambda(v1,m1); Lambda(v2,m2)]) ->
        let assign = expand vars m1
        Expr.Lambda(v1, expand (Map.add v2 (expand vars assign) vars) m2)
        | _ -> expanded

    /// Convert an F# expression to HLSL
    let methodBody expr = 
        let rec methodBody ret = function
        | NewObject(constructorInfo, exprList) ->
            /// When the shader consists of only a single new object expression, is assumed that this object type
            /// is the same as the shader output. 
            match constructorInfo.DeclaringType with
            /// For basic types we assume that this is the shader output. This is the case for pixel shaders.
            | t when t = typeof<float4> -> sprintf "%s float4(%s);" ret (argStr exprList)
            | t when t = typeof<float3> -> sprintf "%s float3(%s);" ret (argStr exprList)
            | objectType -> 
                /// To translate anything other than basic types to HLSL, we need to assign each constructor 
                /// parameter statement to the matching output field. For simplicity we assume that the constructor 
                /// parameters are in the same order as the declared fields.
                /// The results is a sequence of statements of the following form:
                /// o.field1 = constructorExpression1;
                let fieldAssignment(left, right) =
                    sprintf "o.%s = %s;" left right
                let fieldNames =
                    let name(p:PropertyInfo) = p.Name
                    objectType.GetProperties()
                    |> Seq.map name
                let assignments =
                    exprList
                    |> Seq.map hlsl
                    |> Seq.zip fieldNames
                    |> Seq.map fieldAssignment
                    |> String.concat "\n"
                let format = sprintf @"
    %s o;
    %s
    %s o;" 
                format (mapType objectType.Name) assignments ret
        /// Multi-line statements has to start with a let binding
        | Let(var, e1, e2) ->
            let assignment(outer:Var) (right:Expr) =
                match right with 
//                        | Let(inner,e1,e2) -> sprintf "%s\n%s" (assignment inner e1)
//                                                                (assignment outer e2)
                | Let(inner,e1,e2) -> 
                    let format = sprintf @"%s %s;
                    {
                        %s
                    }" 
                    let ret = sprintf "%s =" outer.Name
                    format (mapType outer.Type.Name) 
                            outer.Name
                            (methodBody ret right)
                | _ -> sprintf "%s %s = %s;" (mapType(outer.Type.Name))
                                                outer.Name 
                                                (hlsl right)

            sprintf "%s\n%s" (assignment var e1)
                             (methodBody ret e2)
        | Sequential(e1,e2) -> (hlsl e1) + (methodBody ret e2)
        /// This has to be a single line statement.
        | body -> sprintf "%s %s;" ret (hlsl body)

        and hlsl expr =
            let infix op e1 e2 = sprintf "(%s) %s (%s)" (hlsl e1) op (hlsl e2)
            let methodCall methodName args = 
                sprintf "%s(%s)" (mapMethod methodName) 
                                 (argStr args) 
            let isMatrix t =     
                [typeof<float3x3>;typeof<float4x4>]
                |> List.exists (fun a -> a = t)            
            match expr with
            | Sequential(e1,e2) -> (methodBody "" e1) + (methodBody "" e2)
            | Var(var) -> var.Name
            | Value(obj,t) -> 
                match obj with
                | :? float32 -> String.Format("{0:0.0#####}f", obj)
                | _ -> (string obj)
            | SpecificCall(<@ (%) @>) (_, _, [l;r])-> infix "%" l r
            | SpecificCall(<@ (+) @>) (_, _, [l;r])-> infix "+" l r
            | SpecificCall(<@ (-) @>) (_, _, [l;r])-> infix "-" l r
            | SpecificCall(<@ (/) @>) (_, _, [l;r])-> infix "/" l r
            | SpecificCall(<@ (*) @>) (_, _, [l;r])-> 
                if isMatrix r.Type then
                    methodCall "mul" [l;r]
                else
                    infix "*" l r
            | SpecificCall(<@ (~-) @>) (_, _, [e])-> 
                sprintf "-(%s)" (hlsl e)
            | SpecificCall(<@ (|>) @>) (_, _, exprs)->
                match exprs with
                |[e;Lambda(var, Call(e2,mi, e3))] ->
                    /// Replace the last element in the list with the first expression 
                    let args = e::(e3 |> List.rev |> List.tail) |> List.rev
                    hlsl(Expr.Call(mi, args))
                |[e1;Let(_,e2,Lambda(_,Call(_,mi, e3)))] -> 
                    hlsl(Expr.Call(mi, [e2; e1]))
                | _ -> string exprs
            | Call(Some(FieldGet(_,fi)), methodInfo, args) ->
                sprintf "%s.%s" fi.Name (methodCall methodInfo.Name args)
            | Call(exprOpt, methodInfo, args) ->
                match methodInfo.Name, args with
                | "subtract", [l;r] -> infix "-" r l
                | "subtractFrom", [l;r] -> infix "-" l r
                | name, _ -> methodCall name args
            | PropertyGet(Some(input), pi, _) ->
                sprintf "(%s).%s" (hlsl input) pi.Name
            | NewObject(constructorInfo, exprList) ->
                let t = constructorInfo.DeclaringType
                if isMatrix t then
                    sprintf "(%s)(%s)" t.Name (argStr exprList)
                else
                    sprintf "%s(%s)" t.Name (argStr exprList)
            | FieldGet(Some(e),fi) -> fi.Name
            | ForIntegerRangeLoop(i, first, last, dothis) ->
                let num = function
                | Int32(n) -> string(n)
                | Var(v) -> v.Name
                | _ -> failwith "Expected either an integer or variable in for loop range."
                let formatForLoop = sprintf @"
    for (int %s=%s; %s <= %s; %s++)
    {
        %s
    };"
                let counter = i.Name
                formatForLoop counter (num first) counter (num last) counter (methodBody "" dothis)
            | VarSet(x, expr) ->
                sprintf "%s = %s;\n" x.Name (methodBody "" expr)
            | NewTuple(exprs) ->
                argStr exprs
            | TupleGet(x, i) -> hlsl x
            | expr -> failwith(sprintf "TODO: add support for more expressions like: %A" expr)
        and argStr args =
            args
            |> Seq.map hlsl
            |> String.concat ","
        (* Since we do not support inner functions, we will inline all local function definitions
           by replacing all function definitions with their body, and then replacing the application *)
        let inverted = invertPipelines expr
        let expanded = expand Map.empty inverted
        let ordered = orderLetBindings expanded
        let renamed = renameVars 0 Map.empty ordered 
        methodBody "return" renamed
        (*
        invertPipelines
       // >> renameVars 0 Map.empty
        >> expand Map.empty
        >> orderLetBindings
        >> methodBody "return"*)

    /// A tuple representing the generated HLSL for the shader method as the first value
    /// and the method name as the second.
    let shaderMethods(t:Type) =
        /// Recursively evaluate expressions and translating to HLSL
        let (|EntryPoint|_|) name expr = 
            match name with
            | "pixel" | "vertex" -> 
                match expr with
                | Lambda(v, expr) -> Some(expr)
                | _ -> failwith "Unexpected entry point for shader function"
            | _ -> None
        let (|HelperFunction|_|) = function
            | Lambda(_, _) as expr-> 
                let rec gather args expr =
                    match expr with
                    // Ignore the 'this' parameter that is added by the compiler
                    | Lambda(v, expr) when v.Name = "this" -> gather args expr
                    | Lambda(v, expr) -> gather (v::args) expr
                    | _ -> List.rev args, expr
                Some(gather [] expr)
            | _ -> None
        let shaderMethod name = function
            | EntryPoint name (Lambda(param, expr)) -> 
                let methodAnnotation = if name = "pixel" then " : SV_TARGET" else ""
                sprintf "%s %s(%s %s)%s{ %s };"   (expr.Type.Name |> mapType)
                                                name
                                                (param.Type.Name |> mapType)
                                                param.Name
                                                methodAnnotation
                                                (methodBody expr)
            | HelperFunction(args,expr)-> 
                let rec parameters str = function
                | [] -> sprintf "%s" str
                | x:Var::[] -> 
                    let t = x.Type.Name |> mapType
                    sprintf "%s%s %s" str t x.Name
                | x::xs -> 
                    let t = x.Type.Name |> mapType
                    let str = sprintf "%s%s %s," str t x.Name 
                    parameters str xs

                sprintf "%s %s(%s){ %s };"   
                                    (expr.Type.Name |> mapType)
                                    name
                                    (parameters "" args)
                                    (methodBody expr)


        let shader shaderMethodInfo =
            match shaderMethodInfo with
            | MethodWithReflectedDefinition(expr) -> 
                shaderMethod shaderMethodInfo.Name expr
            | _ -> failwith "Expected method with reflected definition..."
        
        let methodName(mi:MethodInfo) = mi.Name
        //let isReflected(mi:MethodInfo) = not(mi.GetCustomAttribute(typeof<ShaderFunction>) = null)
        let isEntry(mi:MethodInfo) = mi.Name = "pixel" || mi.Name = "vertex"
        methods t
        |> Seq.filter isEntry
        |> Seq.map shader


    /// Produce the complete shader by aggregating all the parts
    let toHLSL(t:Type) =
        seq { yield! constants t
              yield! inputStructs t
              yield! textures t
              yield! samplers t
              yield! shaderMethods t }
        |> String.concat "\n"

