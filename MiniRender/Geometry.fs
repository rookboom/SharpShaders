namespace SharpShaders

open SharpDX
open System

module Geometry =
    let pi = float32(Math.PI)
    let piBy2 = pi / 2.0f
    let twoPi = 2.0f * pi
    /// We start by defining the vertices that form one face which consits of two triangles
    let private face =    [|Vector3(0.0f,0.0f,0.0f) 
                            Vector3(1.0f,0.0f,0.0f)
                            Vector3(0.0f,1.0f,0.0f)
                            Vector3(0.0f,1.0f,0.0f)
                            Vector3(1.0f,0.0f,0.0f)
                            Vector3(1.0f,1.0f,0.0f)|]

    /// Create a quad and transform to produce a vertex array.
    /// We construct our return vertex as a tuple (position,normal,uv), 
    /// so not to bind ourselves 
    /// to any particular memory layout. Consumers of this method can decide
    /// which components of position, normal, vertex are interesting for the
    /// shader they will use and map it to the appropriate struct
    let private transformedFace translation rotation scale =
        let translationMatrix = Matrix.Translation(translation)
        let rotationMatrix = 
            let yaw, pitch, roll = rotation
            Matrix.RotationYawPitchRoll(yaw,pitch,roll)
        let scalingMatrix = Matrix.Scaling(ref scale)
        let transform = translationMatrix*rotationMatrix*scalingMatrix
        let positions = 
            let transformCoordinate(v:Vector3) = 
                Vector3.TransformCoordinate(v, transform)
            face |> Array.map transformCoordinate

        /// The positions of the face were conveniently chosen to also be the
        /// the texture coordinates.
        let uvs = 
            let uv(v:Vector3) = Vector2(v.X,v.Y)
            face |> Array.map uv

        /// Each face has a single normal which will be duplicated per vertex
        let normal = Vector3.TransformCoordinate(Vector3.UnitZ, rotationMatrix)
        let vertex (pos,uv) = (pos,normal,uv)

        Array.zip positions uvs
        |> Array.map vertex 

    let quad scale = 
        let scaling =
            let x,y = scale
            Vector3(x,y,1.0f)
        /// The default camera orientation is on the z-axis in the direction of positive z
        /// Rotate the quad so it faces the default camera orientation
        let yawPitchRoll = (pi, 0.0f,0.0f)
        let translation = Vector3(-0.5f,-0.5f,0.0f)
            
        transformedFace translation yawPitchRoll scaling

    let cube scale =
        let rotatedFacePitchYaw pitch yaw =
            let uniformScaling = Vector3(scale,scale,scale) 
            // We want to arange the face equadistant from the origin
            // In order for the cube to have unit length on each size, we have to translate
            // the face by 0.5 in the z-axis. After rotating the face by 180 degrees,
            // the distance between the faces will have unit length.
            let translation = Vector3(-0.5f,-0.5f,0.5f)
            transformedFace translation (yaw, pitch, 0.0f) uniformScaling
        /// Create the sides of the cube by rotating the front face by 90 degrees four times 
        /// around the Y axis. Note that we start at zero in order to transform the first
        /// face from a postion array to a 3 component vertex array.
        let sides =
            [|0.0f; piBy2; pi; pi + piBy2|]
            |> Array.collect(rotatedFacePitchYaw 0.0f)

        // Create the top and bottom by rotating the front face 90 and -90 degrees about the X axis
        let top, bottom = rotatedFacePitchYaw(piBy2) 0.0f, rotatedFacePitchYaw(-piBy2) 0.0f
        Array.concat (  seq {   yield sides
                                yield top
                                yield bottom }   )
            

