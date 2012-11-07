namespace SharpShaders

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
