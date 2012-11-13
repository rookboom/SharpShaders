namespace SharpShaders.Shaders

open SharpDX
open SharpShaders.Math
open SharpShaders

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

    [<ReflectedDefinition>]
    type Shader(obj:ObjectConstants, mat:MaterialConstants) =

        [<VertexShader>]
        member m.vertex(input:VSInput) =
            PSInput(input.Position * obj.WorldViewProjection)

        [<PixelShader>]
        member m.pixel(input:PSInput) =
            float4(1.0f, 0.0f, 1.0f,1.0f)
