namespace SharpShaders.Shaders

open SharpDX
open SharpShaders.Math
open System.Runtime.InteropServices
open SharpShaders

module Diffuse =
    [<Struct; StructLayout(LayoutKind.Explicit, Size=16)>]
    type SceneConstants =
        [<FieldOffset(0)>]  val LightDirection : float3
        with
        new(lightDir) = { LightDirection = lightDir}

    [<Struct; StructLayout(LayoutKind.Explicit, Size=16)>]
    type MaterialConstants =
        [<FieldOffset(0)>]  val Diffuse : float3
        with
        new(diffuse) = { Diffuse = diffuse}

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type ObjectConstants(wvp:float4x4, w:float4x4) =
        member m.WorldViewProjection = wvp
        member m.World = w
    
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type VSInput(p:float4, n:float3) =
        member m.Position = p
        member m.Normal = n

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type PSInput(p:float4, n:float3) =
        member m.PositionHS = p
        member m.Normal = n

    [<ShaderMethod>]
    let color lightDirection (materialDiffuse:float3) normal =  
        normal
        |> normalize  
        |> dot -lightDirection
        |> mul materialDiffuse
        |> saturate

    type Shader(scene:SceneConstants,
                obj:ObjectConstants,
                mat:MaterialConstants) =

        [<ShaderMethod>]
        member m.vertex(input:VSInput) =
            PSInput(input.Position * obj.WorldViewProjection,
                    input.Normal * float3x3(obj.World))    

        [<ShaderMethod>]
        member m.pixel(input:PSInput) =
            let final = color scene.LightDirection mat.Diffuse input.Normal
            float4(final, 1.0f)


