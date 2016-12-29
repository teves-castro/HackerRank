// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open System
open System.Diagnostics
open System.IO
open Fake
open Fake.Testing
open Fake.Git
open Fake.ProcessHelper
open Fake.ReleaseNotesHelper
open Fake.ZipHelper

// Directories
let buildDir  = "./dist/build/"
let testDir  = "./dist/test/"
let deployDir = "./dist/deploy/"

// Filesets
let appReferences  =
    !! "/src/app/**/*.fsproj"

let testReferences  =
    !! "/src/test/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug testDir "Build" testReferences
    |> Log "TestBuild-Output: "
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

Target "Test" (fun _ ->
    !! (testDir + "/Tests.dll")
    |> NUnit3 (fun (p: NUnit3Params) ->
        {p with
            ResultSpecs = [testDir + "Results.xml"]
            ToolPath = "./packages/NUnit.ConsoleRunner/tools/nunit3-console.exe"})
)

// Build order
"Clean"
    ==> "Build"
    ==> "BuildTest"
    ==> "Test"
    ==> "Deploy"

// start build
RunTargetOrDefault "Build"
