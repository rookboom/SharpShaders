(* Copyright (c) 2012 SharpShaders - Johan Verwey
   See the file license.txt for copying permission. *)
namespace SharpShaders.Shaders

open SharpDX
open SharpDX.Direct3D11
open System.Runtime.InteropServices
open SharpShaders.Math
open SharpShaders

module BlinnPhong =
   [<Struct; StructLayout(LayoutKind.Explicit, Size=48)>]
    type SceneConstants_IfYouDoNotHavePostSharp =
        [<FieldOffset(0)>]  val Eye             : float3
        [<FieldOffset(16)>] val Light  : float3
        [<FieldOffset(32)>] val AmbientLight    : float3
        [<FieldOffset(44)>] val LightRangeSquared  : float32
        with
        new(eye, light, ambientLight, lightRange) = { 
            Eye = eye
            Light = light
            AmbientLight = ambientLight
            LightRangeSquared = lightRange }

   [<Struct; ConstantPacking>]
    type SceneConstants(eye:float3, light:float3, ambientLight:float3, lightRangeSquared:float32) =
        member m.Eye = eye
        member m.Light = light
        member m.AmbientLight = ambientLight
        member m.LightRangeSquared = lightRangeSquared
        
    [<Struct; ConstantPacking>]
    type MaterialConstants(diffuse:float3, specular:float3,shine:float32)  =
        member m.Diffuse = diffuse
        member m.Specular = specular
        member m.Shine = shine

    [<Struct; ConstantPacking>]
    type ObjectConstants(wvp:float4x4, w:float4x4) =
        member m.WorldViewProjection = wvp
        member m.World = w
    
    [<Struct>]
    type VSInput(p:float4, n:float3, uv:float2) =
        member m.Position = p
        member m.Normal = n
        member m.UV = uv

    [<Struct>]
    type PSInput(p:float4, wp:float3, n:float3, uv:float2) =
        member m.PositionHS = p
        member m.PositionWS = wp
        member m.Normal = n
        member m.UV = uv

    [<ReflectedDefinition>]
    let surfaceColor (scene:SceneConstants) (mat:MaterialConstants) worldPos normal =

        let lightVec = scene.Light - worldPos 
        let lightDir = normalize lightVec
        let lightFallOff = 
            let lightVecSquared = (lightVec |> dot lightVec)
            scene.LightRangeSquared/lightVecSquared
            |> saturatef
        let diffuse = 
            normal 
            |> dot lightDir
            |> max 0.0f
            |> mul mat.Diffuse
            |> mul lightFallOff
            |> saturate
        let specular = 
            let viewer = scene.Eye - worldPos
                         |> normalize
            let half_vector = (lightDir + viewer)
                              |> normalize
            half_vector 
            |> dot normal
            |> max 0.0f
            |> pow mat.Shine
            |> mul mat.Specular
            |> mul lightFallOff
            |> saturate

        scene.AmbientLight + diffuse + specular
        |> saturate
        |> withAlpha 1.0f

    [<ReflectedDefinition>]
    type Shader(scene:SceneConstants,
                obj:ObjectConstants,
                mat:MaterialConstants, 
                diffuseTexture:Texture, 
                linearSampler:SamplerStateDescription) =

        [<VertexShader>]
        member m.vertex(input:VSInput) =
            let worldPos = input.Position * obj.World
            PSInput(input.Position * obj.WorldViewProjection,
                    worldPos.xyz,
                    input.Normal * float3x3(obj.World),
                    input.UV)    

        [<PixelShader>]
        member m.pixel(input:PSInput) =
            let tex = diffuseTexture.Sample(linearSampler, input.UV)
            let surface = surfaceColor  scene
                                        mat
                                        input.PositionWS
                                        (normalize input.Normal)
                                    
            tex * surface


