namespace SharpShaders

open System
open System.Runtime.InteropServices
open SharpDX

module Math =

    let inline clamp low high x = max (min x high) low
    let inline scale s v = s*v
    let inline add a b = a + b
    let inline subtractFrom a b = a - b
    let inline subtract a b = b - a
    let inline mul a b = a * b
    let inline pow a b = Math.Pow(float a,float b) |> float32
    let pi = float32(Math.PI)

    type System.Single with
        member m.saturate() = clamp 0.0f 1.0f m

    type Color4 with
        member m.ToBgra() =
            let bgra = Color4(m.Blue,m.Green,m.Red,m.Alpha)
            bgra.ToArgb()

    [<Struct>]
    type float2 =
        val x : float32 
        val y : float32 
        new (x,y) = { x = x; y = y}
        static member zero = float2(0.0f,0.0f)
        static member (+) (v1:float2, v2:float2) = 
            float2(v1.x + v2.x, v1.y + v2.y)
        static member (-) (v1:float2, v2:float2) = 
            float2(v1.x - v2.x, v1.y - v2.y)
        static member (*) (s:float32, v:float2) =
            float2(s*v.x, s*v.y)
        static member (*) (v:float2, s:float32) =
            float2(s*v.x, s*v.y)
        member m.lerp(dest:float2, t:float32) = m + (dest - m) * t

    [<Struct>]
    type float3 =
        val x : float32 
        val y : float32 
        val z : float32
        new (x,y,z) = { x = x; y = y; z = z }
        new(v:Vector3) ={ x = v.X; y = v.Y; z = v.Z}
        member m.xy = float2(m.x,m.y)

        static member (%) (v:float3, s:float32) = 
            float3(v.x % s, v.y % s, v.z % s)
        static member (*) (s:float32, v:float3) =
            float3(s*v.x, s*v.y, s*v.z)
        static member (*) (v:float3, s:float32) = s*v
        static member (*) (v1:float3, v2:float3) =
            float3(v1.x*v2.x, v1.y*v2.y, v1.z*v2.z)
        static member (/) (v:float3, s:float32) = 
            float3(v.x/s, v.y/s, v.z/s)
        static member normalize(v:float3) =
            let length = sqrt(v.x*v.x + v.y*v.y + v.z*v.z)
            float3(v.x/length, v.y/length, v.z/length)
        static member (-) (v1:float3, v2:float3) = 
            float3(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)
        static member (-) (v:float3, s:float32) = 
            float3(v.x - s, v.y - s, v.z - s)
        static member (+) (v1:float3, v2:float3) = 
            float3(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
        static member (+) (v:float3, s:float32) = 
            float3(v.x + s, v.y + s, v.z + s)
        static member (~-) (v:float3) = 
            float3(-v.x, -v.y, -v.z)
        static member saturate(v:float3) = float3(v.x.saturate(),
                                                  v.y.saturate(),
                                                  v.z.saturate())
        static member floor(v:float3) = float3(floor v.x, floor v.y, floor v.z)
        member m.normalize() = float3.normalize m
        member m.floor() = float3.floor m
        member m.saturate() = float3.saturate m
        member m.Abs() = float3(abs m.x, abs m.y, abs m.z)
        member m.lerp(dest:float3, t:float32) = m + (dest - m) * t
        static member zero = float3(0.0f,0.0f,0.0f)
       
    let dot(v1:float3) (v2:float3) = v1.x*v2.x + v1.y*v2.y + v1.z*v2.z
    let inline normalize< ^T when ^T :(member normalize : unit -> ^T)> (x:^T) =
        (^T : (member normalize : unit -> ^T) (x))
    let inline floor< ^T when ^T :(member floor : unit -> ^T)> (x:^T) =
        (^T : (member floor : unit -> ^T) (x))

    [<Struct>]
    type float4 =
        val x : float32 
        val y : float32 
        val z : float32 
        val w : float32 
        new (x,y,z,w) = { x = x; y = y; z = z; w = w }
        new (f:float3, w) = { x = f.x; y = f.y; z = f.z; w = w }
        member m.xy = float2(m.x,m.y)
        member m.zw = float2(m.z,m.w)
        static member (*) (v1:float4, v2:float4) =
            float4(v1.x*v2.x, v1.y*v2.y, v1.z*v2.z, v1.w*v2.w)
        new(v:Vector4) ={ x = v.X; y = v.Y; z = v.Z; w = v.W }

        static member (*) (s:float32, v:float4) =
            float4(s*v.x, s*v.y, s*v.z, s*v.w)
        static member (*) (v:float4, s:float32) = s*v
        static member (/) (v:float4, s:float32) = 
            float4(v.x/s, v.y/s, v.z/s, v.w/s)
        static member (+) (v1:float4, v2:float4) = 
            float4(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z, v1.w + v2.w)
        static member (+) (v:float4, s:float32) = 
            float4(v.x + s, v.y + s, v.z + s, v.w + s)
        static member (-) (v1:float4, v2:float4) = 
            float4(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z, v1.w - v2.w)
        member m.xyz = float3(m.x, m.y, m.z)
        member m.rgb = m.xyz
        member m.a = m.w
        static member zero = float4(0.0f,0.0f,0.0f,1.0f)
        member m.saturate() = float4.saturate m
        member m.lerp(dest:float4, t:float32) = m + (dest - m) * t
        static member saturate(v:float4) = float4(v.x.saturate(),
                                                  v.y.saturate(),
                                                  v.z.saturate(),
                                                  v.w.saturate())
        static member toRgba(col:float4) = Color4(col.x,col.y,col.z,col.w)
        override m.ToString() =
            sprintf "(%3.3f, %3.3f, %3.3f, %3.3f)" m.x m.y m.z m.w 


    let inline saturate< ^T when ^T :(member saturate : unit -> ^T)> (x:^T) =
        (^T : (member saturate : unit -> ^T) (x))
    let inline noise< ^T when ^T :(member noise : unit -> float32)> (x:^T) =
        (^T : (member noise : unit -> float32) (x))
    let inline lerp< ^T when ^T :(member lerp : ^T*float32 -> ^T)> (x:^T, d:^T,t:float32) =
        (^T : (member lerp : ^T*float32 -> ^T) (x,d,t))
    let saturatef(x:float32) = x.saturate()
    let lerpf(x,y,t:float32) = x + (y - x) * t

    [<Struct>]
    type int3(_x:int,_y:int,_z:int) =
        member m.x = _x
        member m.y = _y
        member m.z = _z
        new(f:float3) = int3(int f.x, int f.y, int f.z)

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type float4x4 =
        val M11 : float32 
        val M12 : float32 
        val M13 : float32 
        val M14 : float32 
        val M21 : float32 
        val M22 : float32 
        val M23 : float32 
        val M24 : float32 
        val M31 : float32 
        val M32 : float32 
        val M33 : float32
        val M34 : float32
        val M41 : float32 
        val M42 : float32 
        val M43 : float32
        val M44 : float32
        new (m11, m12, m13, m14, 
             m21, m22, m23, m24,
             m31, m32, m33, m34,
             m41, m42, m43, m44) = {
            M11 = m11; M12 = m12; M13 = m13; M14 = m14;
            M21 = m21; M22 = m22; M23 = m23; M24 = m24;
            M31 = m31; M32 = m32; M33 = m33; M34 = m34;
            M41 = m41; M42 = m42; M43 = m43; M44 = m44; }
        new (m:Matrix) =   float4x4(m.M11, m.M12, m.M13, m.M14,
                                    m.M21, m.M22, m.M23, m.M24,
                                    m.M31, m.M32, m.M33, m.M34,
                                    m.M41, m.M42, m.M43, m.M44)

        static member (*) (v:float3, m:float4x4) =
            float3(v.x*m.M11 + v.y*m.M12 + v.z*m.M13,
                   v.x*m.M21 + v.y*m.M22 + v.z*m.M23,
                   v.x*m.M31 + v.y*m.M32 + v.z*m.M33 )
        static member (*) (v:float4, m:float4x4) =
            float4(v.x*m.M11 + v.y*m.M21 + v.z*m.M31 + v.w*m.M41,
                   v.x*m.M12 + v.y*m.M22 + v.z*m.M32 + v.w*m.M42,
                   v.x*m.M13 + v.y*m.M23 + v.z*m.M33 + v.w*m.M43,
                   v.x*m.M14 + v.y*m.M24 + v.z*m.M34 + v.w*m.M44 )
        static member identity = float4x4(1.0f,0.0f,0.0f,0.0f,
                                          0.0f,1.0f,0.0f,0.0f,
                                          0.0f,0.0f,1.0f,0.0f,
                                          0.0f,0.0f,0.0f,1.0f)

    [<Struct>]
    type float3x3 =
        val M11 : float32 
        val M12 : float32 
        val M13 : float32 
        val M21 : float32 
        val M22 : float32 
        val M23 : float32 
        val M31 : float32 
        val M32 : float32 
        val M33 : float32
        new (m11, m12, m13, m21, m22, m23, m31, m32, m33) = {
            M11 = m11; M12 = m12; M13 = m13;
            M21 = m21; M22 = m22; M23 = m23;
            M31 = m31; M32 = m32; M33 = m33; }
        new (m:float4x4) = float3x3(m.M11, m.M12, m.M13, m.M21, m.M22, m.M23, m.M31, m.M32, m.M33)
        static member identity = float3x3(1.0f,0.0f,1.0f,
                                          0.0f,1.0f,0.0f,
                                          0.0f,0.0f,1.0f)
        static member (*) (v:float3, m:float3x3) =
             float3(v.x*m.M11 + v.y*m.M12 + v.z*m.M13,
                    v.x*m.M21 + v.y*m.M22 + v.z*m.M23,
                    v.x*m.M31 + v.y*m.M32 + v.z*m.M33)
        static member (*) (v:float4, m:float3x3) =
            float3(v.x,v.y,v.z) * m
        
    let fromMatrix(m:Matrix) = float4x4(m.M11, m.M12, m.M13, m.M14,
                                        m.M21, m.M22, m.M23, m.M24,
                                        m.M31, m.M32, m.M33, m.M34,
                                        m.M41, m.M42, m.M43, m.M44)


