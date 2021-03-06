﻿#r @"../packages/FAKE/tools/FakeLib.dll"
#load "./TaskDefinitionHelper.fsx"
#load "./MyConsoleTraceListener.fsx"
#load "./AppveyorEx.fsx"

open Fake
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open Fake.Testing.NUnit3
open Fake.Testing.Expecto
open System
open System.IO
open BlackFox

#load "../packages/SourceLink.Fake/tools/Fake.fsx"

let configuration = "Release"
let rootDir = Path.GetFullPath(__SOURCE_DIRECTORY__ </> "..")
let artifactsDir = rootDir </> "artifacts"
let appBinDir = artifactsDir </> "bin" </> "FsJavaProps" </> configuration

let project = "FsJavaProps"
let summary = "FsJavaPropsr"
let solutionFile  = rootDir </> project + ".sln"
let testAssemblies = artifactsDir </> "bin" </> "*.Tests" </> configuration </> "*.Tests.dll"
let allProjects = !! (rootDir </> "src/**/*.??proj")
let sourceProjects = allProjects -- "src/*.Tests/**.??proj"

/// The profile where the project is posted
let gitOwner = "vbfox"
let gitHome = "https://github.com/" + gitOwner

/// The name of the project on GitHub
let gitName = "FsJavaProps"

/// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" ("https://raw.github.com/" + gitOwner)

// --------------------------------------------------------------------------------------
// Build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release =
    let fromFile = LoadReleaseNotes (rootDir </> "Release Notes.md")
    if buildServer = AppVeyor then
        let appVeyorBuildVersion = int appVeyorBuildVersion
        let nugetVer = sprintf "%s-appveyor%04i" fromFile.NugetVersion appVeyorBuildVersion
        let asmVer = System.Version.Parse(fromFile.AssemblyVersion)
        let asmVer = System.Version(asmVer.Major, asmVer.Minor, asmVer.Build, appVeyorBuildVersion)
        ReleaseNotes.New(asmVer.ToString(), nugetVer, fromFile.Date, fromFile.Notes)
    else
        fromFile

AppVeyorEx.updateBuild (fun info -> { info with Version = Some release.AssemblyVersion })

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

// Generate assembly info files with the right version & up-to-date information
Task "AssemblyInfo" ["?Clean"] <| fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title (projectName)
          Attribute.Product project
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    let tempFile = System.IO.Path.GetTempFileName()

    let updateIfNecessary fromPath toPath =
        let fromText = File.ReadAllText(fromPath)
        let toText = File.ReadAllText(toPath)
        if not (fromText.Equals(toText, StringComparison.InvariantCulture)) then
            CopyFile toPath fromPath

    allProjects
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj ->
            CreateFSharpAssemblyInfo tempFile attributes
            updateIfNecessary tempFile (folderName </> "AssemblyInfo.fs")
        | Csproj ->
            CreateCSharpAssemblyInfo tempFile attributes
            updateIfNecessary tempFile (folderName </> "Properties" </> "AssemblyInfo.cs")
        | Vbproj ->
            CreateVisualBasicAssemblyInfo tempFile attributes
            updateIfNecessary tempFile (folderName </> "My Project" </> "AssemblyInfo.vb")
        )

// --------------------------------------------------------------------------------------
// Clean build results

Task "Clean" [] <| fun _ ->
    CleanDir artifactsDir

    allProjects
    |> MSBuildReleaseExt "" [ "GenerateFullPaths", "True" ] "Clean"
    |> ignore

// --------------------------------------------------------------------------------------
// Build library & test project

Task "Build" ["AssemblyInfo"] <| fun _ ->
    allProjects
    |> MSBuildReleaseExt "" [ "GenerateFullPaths", "True" ] "Build"
    |> ignore

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Task "RunTests" [ "Build"] <| fun _ ->
    !! testAssemblies
        |> Expecto (fun p ->
            {p with
                PrintVersion = false
                FailOnFocusedTests = true })

// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries http://ctaggart.github.io/SourceLink/

open SourceLink

Task "SourceLink" [ "Build" ] <| fun _ ->
    if directoryExists (rootDir </> ".git") then
        let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw gitName
        tracefn "SourceLink base URL: %s" baseUrl

        sourceProjects
        |> Seq.iter (fun projFile ->
            let projectName = Path.GetFileNameWithoutExtension projFile
            let proj = VsProj.LoadRelease projFile
            tracefn "Generating SourceLink for %s on pdb: %s" projectName proj.OutputFilePdb
            SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb rootDir baseUrl
        )
    else
        tracefn "Git isn't used as VCS for this project, SourceLink is disabled"

let finalBinaries = if isMono then "Build" else "SourceLink"

Task "FinalBinaries" [ finalBinaries ] DoNothing

// --------------------------------------------------------------------------------------
// Build a Zip package

let zipPath = artifactsDir </> (sprintf "%s-%s.zip" project release.NugetVersion)

Task "Zip" ["FinalBinaries"] <| fun _ ->
    let comment = sprintf "%s v%s" project release.NugetVersion
    let files =
        !! (appBinDir </> "*.dll")
        ++ (appBinDir </> "*.config")
        ++ (appBinDir </> "*.exe")
    ZipHelper.CreateZip appBinDir zipPath comment 9 false files

    AppVeyor.PushArtifact (fun p ->
        { p with
            Path = zipPath
            FileName = Path.GetFileName(zipPath)
            DeploymentName = "Binaries"
        })

// --------------------------------------------------------------------------------------
// Build a NuGet package

Task "NuGet" ["FinalBinaries"] <| fun _ ->
    Paket.Pack <| fun p ->
        { p with
            OutputPath = artifactsDir
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes
            WorkingDir = appBinDir }

    !! (artifactsDir </> "*.nupkg")
    |> AppVeyor.PushArtifacts

Task "PublishNuget" ["NuGet"] <| fun _ ->
    let key =
        match environVarOrNone "nuget-key" with
        | Some(key) -> key
        | None -> getUserPassword "NuGet key: "

    Paket.Push <| fun p ->  { p with WorkingDir = artifactsDir; ApiKey = key }

// --------------------------------------------------------------------------------------
// Release Scripts

#load "../paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"

Task "GitRelease" [] <| fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

Task "GitHubRelease" ["Zip"] <| fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "GitHub Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "GitHub Password or Token: "

    // release on github
    Octokit.createClient user pw
    |> Octokit.createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> Octokit.uploadFile zipPath
    |> Octokit.releaseDraft
    |> Async.RunSynchronously

// --------------------------------------------------------------------------------------
// Empty targets for readability

Task "Default" ["RunTests"] DoNothing
Task "Release" ["GitHubRelease"; "PublishNuget"] DoNothing
Task "CI" ["Clean"; "RunTests"; "Zip"; "NuGet"] DoNothing

// --------------------------------------------------------------------------------------
// Go! Go! Go!

RunTaskOrDefault "Default"
