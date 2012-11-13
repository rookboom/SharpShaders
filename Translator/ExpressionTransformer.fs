namespace SharpShaders

open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

module ExpressionTransformer =

    //---------------------------------------------------------------------------------------------
    // Renames variables so that inner scope variables will always have a different name than the outer scope
    //---------------------------------------------------------------------------------------------
    let rec private renameVars depth vars  = function
    | Let(v, e1,e2) ->
        let nameClash (k:Var) _ =  k.Name = v.Name
        if vars |> Map.exists nameClash then
            let name =
                sprintf "%s%s" (String.init depth (fun i -> "_") ) v.Name
            let v2 = Var(name, v.Type)
            let newVars = (Map.add v v2 vars)
            Expr.Let(v2, renameVars (depth+1) newVars e1, renameVars depth newVars e2)
        else
            let newVars = (Map.add v v vars)
            Expr.Let(v, renameVars (depth+1) newVars e1, renameVars depth newVars e2)
    | ExprShape.ShapeVar v when Map.containsKey v vars -> Expr.Var(vars.[v])
    | ExprShape.ShapeVar v -> Expr.Var v
    | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v,renameVars depth vars expr)
    | ExprShape.ShapeCombination(o, exprs) ->
        ExprShape.RebuildShapeCombination(o, List.map (renameVars depth vars) exprs)
    
    //---------------------------------------------------------------------------------------------
    let rec invertPipelines = function
    | SpecificCall(<@ (|>) @>) (_, _, exprs)->
        match exprs with
        |[e;Lambda(var, Call(e2,mi, e3))] ->
            /// Replace the last element in the list with the first expression 
            let args = e::(e3 |> List.rev |> List.tail) |> List.rev
            invertPipelines(Expr.Call(mi, args))
        |[e1;Let(_,e2,Lambda(_,Call(_,mi, e3)))] -> 
            invertPipelines(Expr.Call(mi, [e2; e1]))
        |[e1;Let(_,e2,Lambda(_,NewObject(ci, x::xs)))] -> 
            invertPipelines(Expr.NewObject(ci, [e1;e2]))
        | _ -> failwith "unexpected use of pipelining operator"
    | ExprShape.ShapeVar v -> Expr.Var v
    | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v, invertPipelines expr)
    | ExprShape.ShapeCombination(o, exprs) ->
        ExprShape.RebuildShapeCombination(o, List.map invertPipelines exprs)

    //---------------------------------------------------------------------------------------------
    let rec firstLet = function
    | Let(v, e1,e2) as l -> [l]
    | ExprShape.ShapeVar v -> []
    | ExprShape.ShapeLambda(v, expr) -> firstLet expr
    | ExprShape.ShapeCombination(o, exprs) ->
        List.map firstLet exprs
        |> List.concat

    //---------------------------------------------------------------------------------------------
    let rec removeLet = function
    | Let(v, e1,e2) -> e2
    | ExprShape.ShapeVar v -> Expr.Var v
    | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v, removeLet expr)
    | ExprShape.ShapeCombination(o, exprs) ->
        ExprShape.RebuildShapeCombination(o, List.map removeLet exprs)

    //---------------------------------------------------------------------------------------------
    // Renames variables so that inner scope variables will always have a different name than the outer scope
    //---------------------------------------------------------------------------------------------
    let rec private orderLetBindings expr =
        let order parameters args rebuild = 
            let temp = function
                | Let(v,e1,e2), (x:ParameterInfo) ->
                    let name = sprintf "temp_%s" x.Name
                    Expr.Let(Var(name, x.ParameterType),
                             Expr.Let(v,e1, e2), 
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
        // We switch the order of the arguments to the 'pow' function
        // so that the value is curried instead of the power. This allows
        // for more intuitive pipelining i.e To raise a value to the power of 2
        // one can write value |> pow 2
        | Call(None,mi,[e1; e2]) when mi.Name = "pow" ->
            let e = Expr.Call(mi, [e2; e1])
            e
        | Call(None, mi, args)-> 
            let orderedArgs = args |> List.map orderLetBindings
            let rebuild args = Expr.Call(mi, args)
            let parameters = mi.GetParameters()
            let res = order parameters orderedArgs rebuild
            res
        | NewObject(ci, args)->
            let rebuild args = Expr.NewObject(ci, args)
            let parameters = ci.GetParameters()
            order parameters args rebuild

        | ExprShape.ShapeVar v -> Expr.Var v
        | ExprShape.ShapeLambda(v, expr) -> 
            Expr.Lambda(v,orderLetBindings expr)
        | ExprShape.ShapeCombination(o, exprs) ->
            ExprShape.RebuildShapeCombination(o, List.map orderLetBindings exprs)

    //---------------------------------------------------------------------------------------------
    // Create temporary values for external method calls since they
    // cannot easily be inlined inside expressions. The method call is moved to
    // just after the surrounding let binding and the application is assigned to 
    // a temporary variable. This variable is then used in the expression instead
    // of inlining the external method
    //---------------------------------------------------------------------------------------------
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

    //---------------------------------------------------------------------------------------------
    // Expand function will recursively extract all inner functions
    //---------------------------------------------------------------------------------------------
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

    //---------------------------------------------------------------------------------------------
    /// Transorm the expression into a form that is easier to directly translate to HLSL
    let transform expr =
        let expanded = expand Map.empty expr
        let inverted = invertPipelines expanded
        let ordered = orderLetBindings inverted
        renameVars 0 Map.empty ordered 

