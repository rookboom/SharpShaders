namespace SharpShaders.Shaders

open SharpDX
open SharpShaders.Math
open System.Runtime.InteropServices
open SharpShaders

module Marble =
    [<Struct; ConstantPacking>]
    type MaterialConstants(octaves:int32, initialFrequency:float32, amplitude:float32,lacunarity:float32)  =
        member m.Octaves = octaves
        member m.InitialFrequency = initialFrequency
        member m.Amplitude = amplitude
        member m.Lacunarity = lacunarity

    [<Struct>]
    type PSInput(p:float4, wp:float3, n:float3) =
        member m.PositionHS = p
        member m.PositionWS = wp
        member m.Normal = n

    type Shader(scene:Diffuse.SceneConstants,
                obj:Diffuse.ObjectConstants,
                mat:MaterialConstants) =

        [<ShaderMethod>]
        member m.vertex(input:Diffuse.VSInput) =
            let worldPos = input.Position * obj.World
            PSInput(input.Position * obj.WorldViewProjection,
                    worldPos.xyz,
                    input.Normal * float3x3(obj.World))    

        [<ShaderMethod>]
        member m.pixel(input:PSInput) =
            let absNoise = noise >> abs
            let fbmNoise (pos:float3) f =
                let mutable value = 0.0f
                let mutable frequency = mat.InitialFrequency
                let mutable amp = mat.Amplitude
                for i in 1..mat.Octaves do
                    value <- value + amp*f(pos)
                    frequency <- frequency*mat.Lacunarity
                    amp <- amp * mat.Amplitude
                value 
            let diffuse = fbmNoise input.PositionWS absNoise
            let grayMarble = float3(diffuse, diffuse, diffuse)
            let color = Diffuse.color scene.LightDirection grayMarble input.Normal
            float4(color, 1.0f)

    