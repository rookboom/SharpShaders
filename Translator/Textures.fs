(* Copyright (c) 2012 SharpShaders - Johan Verwey
   See the file license.txt for copying permission. *)
namespace SharpShaders

open SharpShaders.Math
open SharpDX.Direct3D11
open System

//=================================================================================================
/// We need a dummy type that represents a texture. We could use a DirectX Texture2D object 
/// but is expensive to instantiate and this class will only be used for unit testing.
//=================================================================================================
type Texture =
    abstract member Sample : SamplerStateDescription*float2 -> float4
    abstract member Sample : SamplerStateDescription*float32 -> float4

//=================================================================================================
type CpuTexture1D(texture:float4[]) =
    let length = Array.length texture 
    interface Texture with
        member m.Sample(sampler:SamplerStateDescription, pos:float2) = 
            failwith "2D index into 1D texture"
            float4.zero
        member m.Sample(sampler:SamplerStateDescription, pos:float32) = 
            texture.[int(pos*float32(length)) % length]

//=================================================================================================
type CpuTexture2D(texture:float4[,]) =
    let length1 = Array2D.length1 texture 
    let length2 = Array2D.length2 texture 
    let sample(pos:float2) =
        let sample x length = int(x*float32(length)) % length
        texture.[sample pos.x length1, sample pos.y length2]
    interface Texture with
        member m.Sample(sampler:SamplerStateDescription, pos:float2) = 
            sample pos
        member m.Sample(sampler:SamplerStateDescription, pos:float32) = 
            sample(float2(pos, 0.0f))


