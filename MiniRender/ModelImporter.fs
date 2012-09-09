namespace SharpShaders

open System
open System.IO
open System.Reflection
open Assimp
open Assimp.Configs

module ModelImporter =
    type Import(scene, importer:IDisposable) =
        member m.Scene = scene
        interface IDisposable with
            member m.Dispose() = importer.Dispose()

    let import filename =
            let path = 
                let dir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
                Path.Combine(dir, filename)

            let importer = new AssimpImporter();

            let config = new NormalSmoothingAngleConfig(66.0f);
            
            importer.SetConfig(config);

            let logstream = 
                let logCallback(msg:string) (userData:nativeint) = Console.WriteLine(msg)
                LogStream(LogStreamCallback(logCallback))
            
            importer.AttachLogStream(logstream)

            let scene = importer.ImportFile(path, PostProcessPreset.TargetRealTimeFast  ||| 
                                                  PostProcessPreset.ConvertToLeftHanded )

            new Import(scene, importer) 

