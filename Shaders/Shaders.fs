namespace SharpShaders.Shaders

open SharpDX
open SharpDX.Direct3D11
open System.Runtime.InteropServices
open SharpShaders.Math
open SharpShaders

type ShaderMethod = ReflectedDefinitionAttribute

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

   [<Struct;ConstantPacking>]
    type SceneConstants_IfPostSharpDidItsJob(eye:float3, light:float3, ambientLight:float3, lightRangeSquared:float32) =
        member m.Eye = eye
        member m.Light = light
        member m.AmbientLight = ambientLight
        member m.LightRangeSquared = lightRangeSquared
        
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
        [<ShaderMethod>]
        member m.vertex(input:VSInput) =
            let worldPos = input.Position * obj.World
            PSInput(input.Position * obj.WorldViewProjection,
                    worldPos.xyz,
                    input.Normal * float3x3(obj.World),
                    input.UV)    

        [<ShaderMethod>]
        member m.pixel(input:PSInput) =
            let worldPos = input.PositionWS
            let normal = input.Normal
                         |> normalize
            let lightVec = worldPos - scene.Light
            let lightDir = normalize lightVec
            let diffuse = 
                let lightFallOff = 
                    let lightVecSquared = (lightVec |> dot lightVec)
                    scene.LightRangeSquared/lightVecSquared
                    |> saturatef
                normal 
                |> dot -lightDir
                |> mul mat.Diffuse
                |> mul lightFallOff
                |> saturatef
            let specular = scene.Eye - worldPos
                           |> normalize
                           |> subtractFrom lightDir
                           |> normalize
                           |> dot normal
                           |> saturatef
                           |> pow mat.Shine
                           |> mul mat.Specular
                           |> saturatef

            let lightColor = float3(1.0f,1.0f,1.0f)
            let intensity = scene.AmbientLight + (diffuse + specular)*lightColor
                            |> saturate
            
            let tex = diffuseTexture.Sample(linearSampler, input.UV)
            let color = tex.rgb * intensity 
            float4(color, 1.0f)

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

    type Shader(scene:SceneConstants,
                obj:ObjectConstants,
                mat:MaterialConstants) =
        let color lightDirection (materialDiffuse:float4) =  
                normalize  
                >> dot -lightDirection
                >> mul materialDiffuse.rgb
                >> saturate

        [<ShaderMethod>]
        member m.vertex(input:VSInput) =
            PSInput(input.Position * obj.WorldViewProjection,
                    input.Normal * float3x3(obj.World))    

        [<ShaderMethod>]
        member m.pixel(input:PSInput) =
            let color = 
                input.Normal 
                |> normalize
                |> dot -scene.LightDirection
                |> mul mat.Diffuse
                |> saturate
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

        [<ShaderMethod>]
        member m.vertex(input:VSInput) =
            PSInput(input.Position * obj.WorldViewProjection)

        [<ShaderMethod>]
        member m.pixel(input:PSInput) =
            float4(1.0f, 0.0f, 1.0f,1.0f)

