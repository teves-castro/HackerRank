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
let binDirs = !! "./src/**/bin" ++ "./src/**/obj"

// Filesets
let appReferences  =
    !! "/src/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs binDirs
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug null "Build" appReferences
    |> Log "AppBuild-Output: "
)

// Build order
"Clean"
    ==> "Build"

// start build
RunTargetOrDefault "Build"
