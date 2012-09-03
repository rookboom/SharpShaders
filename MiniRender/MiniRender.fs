namespace SharpShaders

open System

open SharpDX
open SharpDX.Direct3D
open SharpDX.Direct3D11
open SharpDX.DXGI
open SharpDX.D3DCompiler
open SharpDX.Windows
//open SharpDXExt
open System.Runtime.InteropServices
open SharpShaders

module MiniRender =
    let private float c = float32(c)/256.0f
    (*type Color with
        member m.rgb = float3(float m.R, float m.G, float m.B)
        member m.rgba = float4(float m.R, float m.G, float m.B, float m.A)
        member m.Color4 = Color4(float m.R, float m.G, float m.B, float m.A)
        *)

    type Direct3D11.Device with 
        member m.CreateConstantBuffer<'a>() = 
            let vsConstSize = sizeof<'a>
            let vsConstBufferDesc = new BufferDescription(
                                        BindFlags = BindFlags.ConstantBuffer,
                                        SizeInBytes = vsConstSize, 
                                        CpuAccessFlags = CpuAccessFlags.Write,
                                        Usage = ResourceUsage.Dynamic)
            new Direct3D11.Buffer( m, vsConstBufferDesc )

    type Direct3D11.DeviceContext with 
        member m.UpdateShaderConstants(constBuffer, (data:'T))  = 
            let _, dataStream = m.MapSubresource(
                                            constBuffer, 
                                            MapMode.WriteDiscard, 
                                            SharpDX.Direct3D11.MapFlags.None)
            dataStream.Write(data)
            m.UnmapSubresource( constBuffer, 0 ) 

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type Vertex(p:Vector3, n:Vector3, uv:Vector2) =
        member m.Position = p
        member m.Normal = n
        member m.UV = uv

    let createScene(windowHandle, width, height, vertices, code, inputElements) =
        let device, swapChain =
            let modeDescription = new ModeDescription(  
                                    Width = width,
                                    Height = height,
                                    Format = Format.R8G8B8A8_UNorm,
                                    RefreshRate = new Rational(60,1))
    
            let swapChainDescription =  new SwapChainDescription(   
                                            BufferCount = 1,
                                            ModeDescription = modeDescription,
                                            Usage = Usage.RenderTargetOutput,
                                            OutputHandle = windowHandle,
                                            SampleDescription = new SampleDescription(Count = 1),
                                            IsWindowed = true,
                                            SwapEffect = SwapEffect.Discard)
    
            let _, device, swapChain = Device.CreateWithSwapChain(
                                                DriverType.Hardware,
                                                DeviceCreationFlags.Debug,
                                                swapChainDescription )
            device, swapChain
   

        let renderTargetView = 
            use renderTarget = Resource.FromSwapChain<Texture2D>(swapChain, 0)
            new RenderTargetView(device, renderTarget)

        //Create a viewport with the same size as the window
        let viewport =
            new Viewport(
                Width = float32(width), 
                Height = float32(height), 
                MinDepth = 0.0f, 
                MaxDepth = 1.0f,
                TopLeftX = 0.0f,
                TopLeftY = 0.0f)
        let deviceContext = device.ImmediateContext

        do deviceContext.OutputMerger.SetTargets([|renderTargetView|] )
        do deviceContext.Rasterizer.SetViewports([|viewport|])

        let setupShaders = 
            let rec compile(code:string) (entry:string) (target:string) =
                    ShaderBytecode.Compile(code, 
                                           entry, 
                                           target, 
                                           ShaderFlags.None, 
                                           EffectFlags.None).Bytecode

            let vsByteCode = compile code "vertex" "vs_5_0"
            let inputLayout =
                new InputLayout(
                    device, 
                    vsByteCode, 
                    inputElements)
            let vertexShader = new VertexShader(device, vsByteCode)

            let pixelShader =
                let psByteCode = compile code "pixel" "ps_5_0"
                new PixelShader(device, psByteCode)
            do deviceContext.InputAssembler.PrimitiveTopology <- 
                PrimitiveTopology.TriangleList
            do deviceContext.InputAssembler.InputLayout <- inputLayout
            do deviceContext.VertexShader.Set(vertexShader)
            do deviceContext.PixelShader.Set(pixelShader)
               
        let vertexBuffer, vertexCount = 
            let vertexSize = sizeof<Vertex>
            let vertexData = 
                let vertex(position,normal,uv) = 
                    Vertex(position, normal, uv)
                vertices |> Array.map vertex
            let vertexBufferDesc =  new BufferDescription( 
                                        BindFlags   = BindFlags.VertexBuffer,
                                        SizeInBytes = vertexSize*(Array.length vertices),
                                        Usage       = ResourceUsage.Immutable)
            let buffer = Direct3D11.Buffer.Create(device, 
                                vertexData, 
                                vertexBufferDesc)
            new VertexBufferBinding(buffer, vertexSize, 0),
            Array.length vertices


        use linearSampler = 
            let desc = SamplerStateDescription( Filter = Filter.MinMagMipLinear,
                                                AddressU = TextureAddressMode.Wrap,
                                                AddressV = TextureAddressMode.Wrap,
                                                AddressW = TextureAddressMode.Wrap,
                                                MaximumAnisotropy = 1,
                                                ComparisonFunction = Comparison.Always,
                                                MaximumLod = System.Single.MaxValue)
            new SamplerState(device, desc)


        do deviceContext.PixelShader.SetSampler(0, linearSampler);

        do deviceContext.InputAssembler.SetVertexBuffers(0, vertexBuffer)

        let paint() = 
            do deviceContext.ClearRenderTargetView(
                renderTargetView,
                Color4(0.2f, 0.2f, 0.5f,1.0f))

            do deviceContext.Draw(vertexCount, 0) 
            
            do swapChain.Present(0, PresentFlags.None) |> ignore

        paint,
        device


