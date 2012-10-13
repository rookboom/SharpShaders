namespace SharpShaders.Shaders

open SharpDX
open SharpShaders.Math
open System.Runtime.InteropServices
open SharpShaders

module Marble =
    [<Struct>]
    type MaterialConstants(octaves:int32, initialFrequency:float32, amplitude:float32,lacunarity:float32)  =
        member m.Octaves = octaves
        member m.InitialFrequency = initialFrequency
        member m.Amplitude = amplitude
        member m.Lacunarity = lacunarity

    type Shader(scene:Diffuse.SceneConstants,
                obj:Diffuse.ObjectConstants,
                mat:MaterialConstants) =

        [<ShaderMethod>]
        member m.vertex(input:Diffuse.VSInput) =
            Diffuse.PSInput(input.Position * obj.WorldViewProjection,
                    input.Normal * float3x3(obj.World))    

        [<ShaderMethod>]
        member m.pixel(input:Diffuse.PSInput) =
            let diffuseColor = float3(1.0f,1.0f,1.0f)
            let color = Diffuse.color scene.LightDirection diffuseColor input.Normal
            float4(diffuseColor, 1.0f)

    