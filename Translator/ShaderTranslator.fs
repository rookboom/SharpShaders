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
open SharpShaders.Mappings

//type ShaderFunction = ReflectedDefinitionAttribute
type PixelShaderAttribute() =
    inherit Attribute()
type VertexShaderAttribute() =
    inherit Attribute()
type GeometryShaderAttribute() =
    inherit Attribute()
type HullShaderAttribute() =
    inherit Attribute()
type DomainShaderAttribute() =
    inherit Attribute()

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
    type MethodInfo with
        member m.hasAttribute<'a>() = not(m.GetCustomAttribute(typeof<'a>) = null)


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
        

    let private isEntry(mi:MethodInfo) = 
        mi.hasAttribute<PixelShaderAttribute>() ||
        mi.hasAttribute<VertexShaderAttribute>() ||
        mi.hasAttribute<GeometryShaderAttribute>() ||
        mi.hasAttribute<HullShaderAttribute>() ||
        mi.hasAttribute<DomainShaderAttribute>()
    /// We need to find all the input parameters to all the methods in the shader class.
    /// We do not need to look at output parameters since any output parameter is also
    /// the input of the next shader in the pipeline. 
    let inputStructs(shaderType:Type) =
        let parameters(mi:MethodInfo) = mi.GetParameters()
        let parameterType(pi:ParameterInfo) = pi.ParameterType
        methods shaderType
        //Only create input structs for entry points.
        |> Seq.filter isEntry
        |> Seq.collect parameters
        |> Seq.map parameterType
        |> Seq.distinct
        |> Seq.map formatStruct


    /// A tuple representing the generated HLSL for the shader method as the first value
    /// and the method name as the second.
    let shaderMethods(t:Type) =
        /// Recursively evaluate expressions and translating to HLSL
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

        let shaderMethod(mi:MethodInfo) = function
            | Lambda(v, Lambda(param, expr)) ->
                let methodAnnotation = 
                    if mi.hasAttribute<PixelShaderAttribute>() then " : SV_TARGET" else ""
                sprintf "%s %s(%s %s)%s{ %s };"   (expr.Type.Name |> mapType)
                                                mi.Name
                                                (param.Type.Name |> mapType)
                                                param.Name
                                                methodAnnotation
                                                (ExpressionTranslator.methodBody expr)
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
                                    mi.Name
                                    (parameters "" args)
                                    (ExpressionTranslator.methodBody expr)
            | _ -> failwith "Unexpected shader function"


        let shader shaderMethodInfo =
            match shaderMethodInfo with
            | MethodWithReflectedDefinition(expr) -> 
                shaderMethod shaderMethodInfo expr
            | _ -> failwith "Expected method with reflected definition..."
        
        let methodName(mi:MethodInfo) = mi.Name

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

