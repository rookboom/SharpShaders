namespace SharpShaders

open System.Collections.Generic

module Mappings =
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
        
    let mapMethod = valueOrKey methodMapping
    let mapType = valueOrKey typeMapping

