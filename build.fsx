open System.Text.RegularExpressions
open Fake.IO
#r "paket:
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Cli
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO


let dockerPath =
    Process.tryFindFileOnPath "docker" |> function  
                                          | Some docker -> docker
                                          | _ -> failwith "Can't find docker"


let extractVersionFromBuild build =                 
                let m = Regex.Match(build, @"\d+\.\d+\.\d+\.\d+")
                if m.Success then Some m.Value                
                else None   

let versionNumber =
    match BuildServer.buildVersion with
    | "LocalBuild" -> 
            printfn "Localbuild detected setting version to 1.0.0.0"
            "1.0.0.0"
    | s -> match extractVersionFromBuild s with
            | Some v -> v
            | None -> "1.0.0.0" 

let install = lazy DotNet.install DotNet.Versions.FromGlobalJson

let inline withWorkDir wd =
    DotNet.Options.lift install.Value
    >> DotNet.Options.withWorkingDirectory wd

Target.create "GenerateAssemblyInfo" (fun _ ->  

            !! "**/AssemblyInfo.fs" 
            |> Seq.iter (fun f ->
                            Fake.DotNet.AssemblyInfoFile.createFSharp f 
                                    [
                                        Fake.DotNet.AssemblyInfo.Version versionNumber
                                        Fake.DotNet.AssemblyInfo.FileVersion versionNumber                                        
                                    ] 
                        )            
        ) 

Target.create "Publish Console App" (fun _ -> 
                    let publishOutput = System.IO.Path.Combine(__SOURCE_DIRECTORY__,  "publish")
                    !! "**/legislativeTablesConsole.fsproj"
                    |> Seq.iter (fun projectPath -> (
                                                     Fake.DotNet.DotNet.publish (fun options -> {options with OutputPath = Some publishOutput; Runtime = (Some "win-x86"); Common = DotNet.Options.Create() }) projectPath
                    )
                    )

)



Target.create "Docker build" (fun _ -> 
    printfn "docker path to %s" dockerPath
    
    let result = Process.execWithResult (fun startInfo ->
       {
           startInfo with 
            FileName = dockerPath
            Arguments = "build --iidfile image.txt . "} ) (System.TimeSpan.FromMinutes 5. ) 
    match result.OK with
    | true -> ()
    | false -> 
        result.Errors |> Seq.iter (fun e ->( printfn "%s" e))
        failwith "Error running docker build"
)

Target.create "DockerRun" (fun _ ->     
    printfn "docker path to %s" dockerPath
    Process.setKillCreatedProcesses false
    
    

    let imageIdString = File.readLine (System.IO.Path.Combine(__SOURCE_DIRECTORY__,  "image.txt")) 
    printfn "%s" imageIdString
    let iamgeId = String.split ':' imageIdString |> Seq.last
    
    printfn "kill set to %b" (Process.shouldKillCreatedProcesses())


    Process.start (fun startInfo ->   {startInfo with FileName = dockerPath; Arguments = (sprintf "run -p 5000:5000 --env-file ./.env/env.list %s" iamgeId) } )
    
)

Target.create "Clean Publish folder" (fun _ -> 
    Fake.IO.Directory.create "./Publish"
    
    !! "./Publish/**/*"
    |> Fake.IO.File.deleteAll
    
    Fake.IO.DirectoryInfo.ofPath "./Publish"
    |> Fake.IO.DirectoryInfo.getSubDirectories
    |> Seq.iter (fun di -> di.FullName |> Fake.IO.Directory.delete)    
)

Target.create "Clean Bundle Folder" (fun _ -> 
    !! "./Bundle/**/*"
    |> Fake.IO.File.deleteAll
    
    let dir = Fake.IO.DirectoryInfo.ofPath "./Bundle"
    match dir.Exists with
    | true -> 
               dir |> Fake.IO.DirectoryInfo.getSubDirectories
               |> Seq.iter (fun di -> di.FullName |> Fake.IO.Directory.delete)
    | false -> ()   
)

Target.create "Copy publish to Bundle" (fun _ ->      
    Fake.IO.Shell.copyRecursiveTo true "./bundle/publish" "./publish/" |> ignore
)

Target.create "Copy bat files to bundle" (fun _ -> 
    Fake.IO.Shell.copyRecursiveTo true "./bundle" "./LegislativeTables/legislativeTablesConsole/batFiles/" |> ignore
)

Target.create "Run Integration Tests" (fun _ -> 
    
    let testArgs = ["-- -dt chrontable -y 2016"
                    "-- -dt chrontablecommencement -y 2016"
                    "-- -dt siboundvolumes -y 2016"
                    "-- -dt siboundvolumescommencement -y 2016"
                    "-- -dt tablevi -y 2016"
                    "-- -dt tablevicommencement -y 2016"
                    "-- -dt aspannualvolume -y 2016"
                    "-- -dt aspannualvolumecommencement -y 2016"
                    "-- -dt scotssiboundvolumes -y 2016"
                    "-- -dt scotssiboundvolumescommencement -y 2016"
                    "-- -dt anawannualvolume -y 2016"
                    "-- -dt anawannualvolumecommencement -y 2016"
                    "-- -dt welshsiboundvolumes -y 2016"
                    "-- -dt welshsiboundvolumescommencement -y 2016"]

    testArgs |> List.iter (fun dt -> 
                            let result = DotNet.exec (withWorkDir "./LegislativeTables/LegislativeTablesConsole") "run" dt

                            if result.ExitCode <> 0 then failwith "test failed"
                            )
    
    // let result = DotNet.exec (withWorkDir "./LegislativeTables/LegislativeTablesConsole") "run" "-- -dt chrontable -y 2016"

    // if result.ExitCode <> 0 then failwith "test failed"
)

// "GenerateAssemblyInfo"
//  ==> "Publish Auth Server"
//  ==> "Docker build"
//  ==> "DockerRun"

"Clean Publish Folder"
==> "Publish Console App"
==> "Clean Bundle Folder"
==> "Copy publish to Bundle"
==> "Copy bat files to bundle"




Target.runOrDefault "Copy bat files to bundle"