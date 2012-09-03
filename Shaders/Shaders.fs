namespace SharpShaders.Shaders

open SharpDX
open SharpDX.Direct3D11
open System.Runtime.InteropServices
open SharpShaders.Math
open SharpShaders

module BlinnPhong =
   [<Struct; StructLayout(LayoutKind.Explicit, Size=48)>]
    type SceneConstants =
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
        
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type MaterialConstants(ambient:float32, diffuse:float32, specular:float32,shine:float32)  =
        member m.Ambient = ambient
        member m.Diffuse = diffuse
        member m.Specular = specular
        member m.Shine = shine

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type ObjectConstants(wvp:float4x4, w:float4x4) =
        member m.WorldViewProjection = wvp
        member m.World = w
    
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type VSInput(p:float4, n:float3, uv:float2) =
        member m.Position = p
        member m.Normal = n
        member m.UV = uv

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type PSInput(p:float4, wp:float3, n:float3, uv:float2) =
        member m.PositionHS = p
        member m.PositionWS = wp
        member m.Normal = n
        member m.UV = uv

    type Shader(scene:SceneConstants,
                obj:ObjectConstants,
                mat:MaterialConstants, 
                diffuseTexture:Texture, 
                linearSampler:SamplerStateDescription) =
        [<ReflectedDefinition>]
        member m.vertex(input:VSInput) =
            let worldPos = input.Position * obj.World
            PSInput(input.Position * obj.WorldViewProjection,
                    worldPos.xyz,
                    input.Normal * float3x3(obj.World),
                    input.UV)    

        [<ReflectedDefinition>]
        member m.pixel(input:PSInput) =
            let worldPos = input.PositionWS
            let lightVec = worldPos - scene.Light
            let lightDir = normalize lightVec
            let diffuse = 
                let lightFallOff = 
                    scene.LightRangeSquared/(lightVec |> dot lightVec)
                    |> saturatef
                input.Normal 
                |> normalize
                |> dot -lightDir
                |> mul mat.Diffuse
                |> mul lightFallOff
                |> saturatef
            let specular = scene.Eye - worldPos
                           |> normalize
                           |> subtract lightDir
                           |> normalize
                           |> dot input.Normal
                           |> saturatef
                           |> pow mat.Shine
                           |> mul mat.Specular

            let lightColor = float3(1.0f,1.0f,1.0f)
            let intensity = scene.AmbientLight + (diffuse + specular)*lightColor
                            |> saturate
            
            let albedo = diffuseTexture.Sample(linearSampler, input.UV)
            let color = albedo.rgb*intensity
            float4(color, 1.0f)

module Simplistic =
    //===================================================
    [<Struct>]
    type ObjectConstants =
        val WorldViewProjection : float4x4
        new (wvp) = { WorldViewProjection = wvp }

    [<Struct>]
    type VSInput =
        val Position : float4
        new (pos) = { Position = pos }

    [<Struct>]
    type PSInput =
        val PositionHS : float4
        new (pos) = { PositionHS = pos }

    [<Struct>]
    type MaterialConstants =
        val MaterialDiffuse : Color4
        new (color) = { MaterialDiffuse = color }

    type Shader(obj:ObjectConstants, mat:MaterialConstants) =

        [<ReflectedDefinition>]
        member m.vertex(input:VSInput) =
            PSInput(input.Position * obj.WorldViewProjection)

        [<ReflectedDefinition>]
        member m.pixel(input:PSInput) =
            float4(1.0f, 0.0f, 1.0f,1.0f)

