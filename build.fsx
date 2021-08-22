#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()

Target.create "Clean" (fun _ -> !! "*/bin" ++ "*/obj" |> Shell.deleteDirs)

Target.create "Build" (fun _ -> !! "*.sln" |> Seq.iter (DotNet.build id))

Target.create
    "Test"
    (fun _ ->
        !! "Tests/*.*sproj"
        |> Seq.iter (
            DotNet.test
                (fun options ->
                    { options with
                          NoBuild = true
                          NoLogo = true
                          Configuration = DotNet.BuildConfiguration.Release })
        ))

Target.create "All" ignore

"Clean" ==> "Build" ==> "Test" ==> "All"

Target.runOrDefault "All"
