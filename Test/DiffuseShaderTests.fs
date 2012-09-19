namespace Flow.Test

open Xunit
open SharpDX
open SharpShaders.Shaders
open SharpShaders.Math

//=======================================================================================
module DiffuseShaderTests =
    [<Fact>]
    let ``The vertex shader should transform a local vertex to screen space``() =
        let sceneConstants = Diffuse.SceneConstants()
        let world = Matrix.Translation(Vector3(25.0f,25.0f,25.0f))
        // let the viewport 100 wide around the origin: from -50 to 50 in xy
        // our translated position of 25 should then be half way to the edge
        // of the screen and map to 0.5, 0.5 in xy
        let viewProjection = Matrix.OrthoLH (100.0f, 100.0f, 0.0f, 100.0f)
        let localPosition = float4.zero
        let localNormal = float3(1.0f, 0.0f, 0.0f)
        // The above world and view matrix should translate into the following
        // homogeneous screenspace coordinates
        let positionHS = float4(0.5f, 0.5f, 0.25f, 1.0f)
        let objectConstants = Diffuse.ObjectConstants(
                                fromMatrix(world*viewProjection),
                                fromMatrix(world))
        let matConstants = Diffuse.MaterialConstants()
        let diffuseShader = Diffuse.Shader(sceneConstants, objectConstants, matConstants)
        let vsInput = Diffuse.VSInput(localPosition, localNormal)
        let psInput = diffuseShader.vertex(vsInput)
        Assert.Equal(positionHS, psInput.PositionHS)      

