open SharpShaders
open SharpShaders.Math
open SharpShaders.Shaders
open SharpDX.Windows
open SharpDX
open SharpDX.Direct3D11
open System.Windows.Forms
open System.Diagnostics
open System.Drawing

let width, height = 640, 480
let form = new Form(Visible = true, Text = "Perlin Noise", Width = width, Height = height)
let saveNoiseSlice =
    use bmp = new Bitmap(512, 512)
    let setColor i j (c:float32) = 
        let shade = int((c+1.0f)/2.0f*255.0f)
        bmp.SetPixel(i, j, Color.FromArgb(255, shade, shade, shade))

    let marbleShader = Marble.Shader(BlinnPhong.SceneConstants(),
                                     BlinnPhong.ObjectConstants(),
                                     BlinnPhong.MaterialConstants(),
                                     CpuTexture2D(PerlinTexture.permutation2D),
                                     CpuTexture2D(PerlinTexture.permutedGradients),
                                     SamplerStateDescription())


    marbleShader.noise2D()
    |> Array2D.iteri setColor
    bmp.Save("noise2D.jpg", Imaging.ImageFormat.Png) |> ignore

let run() = 

    let hlsl = ShaderTranslator.toHLSL typeof<Marble.Shader>
    let inputElements = 
        InputElements.map typeof<Vertex> typeof<Diffuse.VSInput>
        |> Seq.toArray

    System.Diagnostics.Debug.WriteLine(hlsl);
    let eye = Vector3(0.0f,0.0f,-4.0f)
    let sceneConstants, matConstants, objectConstants = 
        let light = Vector3(5.0f, 5.0f, -5.0f)
        let darkRed = float3(0.5f,0.0f,0.0f)
        let darkBlue = float3(0.0f,0.0f,1.0f)
        let darkGreen = float3(0.0f,0.5f,0.0f)
        let gray = float3(0.5f,0.5f,0.5f)

        BlinnPhong.SceneConstants(float3(eye), float3(light), float3(0.1f,0.1f,0.1f), 100.0f),
        BlinnPhong.MaterialConstants(darkBlue, gray, 50.0f),
        BlinnPhong.ObjectConstants(fromMatrix(Matrix.Identity),
                                   fromMatrix(Matrix.Identity))

    use renderer = new MiniRender(form.Handle, width, height)
    let updateObjectConstants = 
        renderer.createVertexShader hlsl inputElements objectConstants
    let updateOtherConstants =
        renderer.createPixelShaderWithEntry "marbled" hlsl (sceneConstants, matConstants)

    let pointSampler = 
        let desc = SamplerStateDescription( Filter = Filter.MinMagMipPoint,
                                            AddressU = TextureAddressMode.Wrap,
                                            AddressV = TextureAddressMode.Wrap,
                                            AddressW = TextureAddressMode.Clamp,
                                            ComparisonFunction = Comparison.Always,
                                            MaximumLod = System.Single.MaxValue)
        renderer.createSampler desc
    let textures =[ 
        (renderer.createTexture2D PerlinTexture.permutation2D DXGI.Format.R8G8B8A8_UNorm, pointSampler);
        (renderer.createTexture2D PerlinTexture.permutedGradients DXGI.Format.R8G8B8A8_UNorm, pointSampler)]

    renderer.setTextures textures
    
    let geometry =
        use import = ModelImporter.import "sphere.obj"
        Geometry.fromMesh import.Scene.Meshes.[0]
    let draw = renderer.createScene(geometry)
    let viewProjection = renderer.createViewProjection(eye)
    let sw = Stopwatch.StartNew()

    let render() =
        let world = 
            let time = float32(sw.ElapsedMilliseconds)/1000.0f
            Matrix.RotationYawPitchRoll(0.25f*time, 0.5f*time, 0.0f)
        BlinnPhong.ObjectConstants(fromMatrix(world*viewProjection), fromMatrix(world))
        |> updateObjectConstants
        draw()
    RenderLoop.Run(form, render)
    
run()
