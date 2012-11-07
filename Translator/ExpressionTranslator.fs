namespace SharpShaders

open SharpShaders.Math
open SharpShaders.Mappings
open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

module ExpressionTranslator =

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
                sprintf "%s = %s\n" x.Name (methodBody "" expr)
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
        expr
        |> ExpressionTransformer.transform
        |> methodBody "return"


