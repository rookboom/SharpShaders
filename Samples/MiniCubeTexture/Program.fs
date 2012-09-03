open SharpShaders
open SharpShaders.Math
open SharpShaders.MiniRender
open SharpShaders.Shaders
open SharpDX.Windows
open SharpDX
open System.Windows.Forms
open System.Diagnostics

let width, height = 640, 480
let form = new Form(Visible = true, Text = "Blue skies", Width = width, Height = height)
let run() = 
    let hlsl = ShaderTranslator.toHLSL typeof<BlinnPhong.Shader>
    let inputElements = 
        ShaderTranslator.inputElements typeof<BlinnPhong.VSInput> typeof<Vertex>
        |> Seq.toArray

    System.Diagnostics.Debug.WriteLine(hlsl);

    let geometry = Geometry.cube 1.0f
    let draw, device = createScene(form.Handle, width, height, geometry, hlsl, inputElements)
    
    let deviceContext = device.ImmediateContext
    let sceneConstBuffer = device.CreateConstantBuffer<BlinnPhong.SceneConstants>()
    let matConstBuffer = device.CreateConstantBuffer<BlinnPhong.MaterialConstants>()
    let objectConstBuffer = device.CreateConstantBuffer<BlinnPhong.ObjectConstants>()
    let eye = Vector3(0.0f,0.0f,-6.0f)

    let sceneConstants = 
        let light = eye
        Shaders.BlinnPhong.SceneConstants(float3(eye), float3(light), float3(0.1f,0.1f,0.1f), 10.0f)
    let matConstants = Shaders.BlinnPhong.MaterialConstants(0.1f, 0.5f, 0.5f, 30.0f)

    do deviceContext.UpdateShaderConstants(sceneConstBuffer, sceneConstants)
    do deviceContext.UpdateShaderConstants(matConstBuffer,  matConstants)
    do deviceContext.PixelShader.SetConstantBuffer(0, sceneConstBuffer)
    do deviceContext.PixelShader.SetConstantBuffer(1, matConstBuffer)

    use textureView = TextureLoader.fromFile device "GeneticaMortarlessBlocks.jpg"
    do deviceContext.PixelShader.SetShaderResource(0, textureView)

    do deviceContext.VertexShader.SetConstantBuffer(0, objectConstBuffer)
    let sw = Stopwatch.StartNew()
    let render() =
        let world = 
            let time = float32(sw.ElapsedMilliseconds)/1000.0f
            Matrix.RotationYawPitchRoll(time, 2.0f*time, 0.0f)
        let viewProjection =
            let lookAt, up = 
                Vector3.Zero, 
                Vector3(0.0f,1.0f,0.0f)
            // Look at the origin
            let view = Matrix.LookAtLH(eye, lookAt, up)
                                  
            let fovy, aspect, near, far = 
                float32(pi)/4.0f, 
                float32(width)/float32(height), 
                1.0f, 
                20.0f 
            let projection = Matrix.PerspectiveFovLH(fovy, aspect, near, far)
            view*projection
        let objectConstants = BlinnPhong.ObjectConstants(fromMatrix(world*viewProjection),
                                                         fromMatrix(world))
        do deviceContext.UpdateShaderConstants(objectConstBuffer, objectConstants)
        draw()
    RenderLoop.Run(form, render)
    
run()
