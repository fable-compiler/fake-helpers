module Fable.FakeHelpers

open System
open System.IO
open System.Text.RegularExpressions
open Fake
open Fake.Git
open Fake.ReleaseNotesHelper

let [<Literal>] RELEASE_NOTES = "RELEASE_NOTES.md"
let [<Literal>] NUGET_PACKAGE_VERSION_TAG_UPPER = "PACKAGEVERSION"
let MSBUILD_VERSION_REGEX = Regex(@"<((?:Package)?Version)>(.*?)<\/\1>", RegexOptions.IgnoreCase)
let NPM_VERSION_REGEX = Regex(@"""version"":\s*""(.*?)""")

let visitFile (visitor: string->string) (fileName : string) =
    // This code is supposed to prevent OutOfMemory exceptions
    // but it outputs wrong BOM
    // use reader = new StreamReader(fileName, encoding)
    // let tempFileName = Path.GetTempFileName()
    // use writer = new StreamWriter(tempFileName, false, encoding)
    // while not reader.EndOfStream do
    //     reader.ReadLine() |> visitor |> writer.WriteLine
    // reader.Close()
    // writer.Close()
    // File.Delete(fileName)
    // File.Move(tempFileName, fileName)
    File.ReadAllLines(fileName)
    |> Array.map (visitor)
    |> fun lines -> File.WriteAllLines(fileName, lines)

let replaceLines (replacer: string->Match->string option) (reg: Regex) (fileName: string) =
    fileName |> visitFile (fun line ->
        let m = reg.Match(line)
        if not m.Success
        then line
        else
            match replacer line m with
            | None -> line
            | Some newLine -> newLine)

let rec findFileUpwards fileName dir =
    let fullPath = dir </> fileName
    if File.Exists(fullPath)
    then fullPath
    else
        let parent = Directory.GetParent(dir)
        if isNull parent then
            failwithf "Couldn't find %s directory" fileName
        findFileUpwards fileName parent.FullName

let run workingDir fileName args =
    printfn "CWD: %s" workingDir
    let fileName, args =
        if EnvironmentHelper.isUnix
        then fileName, args
        else "cmd", ("/C " + fileName + " " + args)
    let ok =
        execProcess (fun info ->
            info.FileName <- fileName
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
    if not ok then
        failwithf "'%s> %s %s' task failed" workingDir fileName args

let loadReleaseNotes projFile =
    let projDir =
        if Path.HasExtension(projFile)
        then Path.GetDirectoryName(projFile)
        else projFile
    findFileUpwards RELEASE_NOTES projDir
    |> ReleaseNotesHelper.LoadReleaseNotes

let needsPublishing silent (readVersionInLine: string->string option) (releaseNotes: ReleaseNotes) projFile =
    let print msg =
        if not silent then
            let projName =
                let projName = Path.GetFileNameWithoutExtension(projFile)
                if projName = "package" // package.json
                then Path.GetFileName(Path.GetDirectoryName(projFile))
                else projName
            printfn "%s > %s" projName msg
    if releaseNotes.NugetVersion.ToUpper().EndsWith("NEXT")
    then
        print "Version in Release Notes ends with NEXT, don't publish yet."
        false
    else
        File.ReadLines(projFile)
        |> Seq.tryPick readVersionInLine
        |> function
            | None -> failwithf "Couldn't find version in %s" projFile
            | Some version ->
                let sameVersion = version = releaseNotes.NugetVersion
                if sameVersion then
                    sprintf "Already version %s, no need to publish" releaseNotes.NugetVersion |> print
                not sameVersion

type Package(projPath, ?build, ?pkgName, ?msbuildProps) =
    member __.ProjectPath: string = projPath
    member __.PackageName: string option = pkgName
    member __.Build: (unit -> unit) option = build
    member __.MsBuildProps: (string*string) list = defaultArg msbuildProps []

let splitPrerelease (version: string) =
    let i = version.IndexOf("-")
    if i > 0
    then version.Substring(0, i), Some(version.Substring(i + 1))
    else version, None

let pushNuget dotnetExePath (releaseNotes: ReleaseNotes) (pkg: Package) (projFile: string) =
    let readVersionInLine line =
        let m = MSBUILD_VERSION_REGEX.Match(line)
        if m.Success && m.Groups.[1].Value.ToUpper() = NUGET_PACKAGE_VERSION_TAG_UPPER
        then Some m.Groups.[2].Value
        else None
    if needsPublishing false readVersionInLine releaseNotes projFile then
        pkg.Build |> Option.iter (fun build -> build())
        let projDir = Path.GetDirectoryName(projFile)
        let nugetKey =
            match environVarOrNone "NUGET_KEY" with
            | Some nugetKey -> nugetKey
            | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"
        // Restore dependencies here so they're updated to latest project versions
        run projDir dotnetExePath "restore"
        // Update the project file
        (MSBUILD_VERSION_REGEX, projFile) ||> replaceLines (fun line m ->
            let version =
                if m.Groups.[1].Value.ToUpper() = NUGET_PACKAGE_VERSION_TAG_UPPER
                then releaseNotes.NugetVersion
                else splitPrerelease releaseNotes.NugetVersion |> fst
            let replacement = String.Format("<{0}>{1}</{0}>", m.Groups.[1].Value, version)
            MSBUILD_VERSION_REGEX.Replace(line, replacement) |> Some)
        try
            let tempDir = projDir </> "temp"
            CleanDir tempDir
            sprintf "pack -c Release -o %s %s"
                (match pkg.MsBuildProps with
                 | [] -> ""
                 | props -> "/property:" + (List.map (fun (k,v) -> k+"="+v) props |> String.concat ";"))
                tempDir
            |> run projDir dotnetExePath
            let pushCmd =
                let pkgName =
                    match pkg.PackageName with
                    | Some name -> name
                    | None -> Path.GetFileNameWithoutExtension(projFile)
                let nupkg =
                    Directory.GetFiles(tempDir)
                    |> Seq.tryFind (fun path -> path.Contains(pkgName))
                    |> Option.defaultWith (fun () -> failwithf "Cannot find .nupgk with name %s" pkgName)
                sprintf "nuget push %s -s nuget.org -k %s" nupkg nugetKey
            run projDir dotnetExePath pushCmd
            CleanDir tempDir
        with _ ->
            Path.GetFileNameWithoutExtension(projFile)
            |> printfn "There's been an error when pushing project: %s"
            printfn "Please revert the version change in .fsproj"
            reraise()

let pushNpm (releaseNotes: ReleaseNotes) build (pkgJson: string) =
    let readVersionInLine line =
        let m = NPM_VERSION_REGEX.Match(line)
        if m.Success then Some m.Groups.[1].Value else None
    let projDir = Path.GetDirectoryName(pkgJson)
    if needsPublishing false readVersionInLine releaseNotes pkgJson then
        build |> Option.iter (fun build -> build())
        run projDir "npm" ("version " + releaseNotes.NugetVersion)
        try
            let publishCmd =
                match splitPrerelease releaseNotes.NugetVersion with
                | _, Some _ -> "publish --tag next"
                | _, None -> "publish"
            run projDir "npm" publishCmd
        with _ ->
            printfn "There's been an error when pushing project: %s" projDir
            printfn "Please revert the version change in package.json"
            reraise()

/// Accepts of list of tuples where the first element is an optional function
/// to be run before publishing the package
let publishPackages2 baseDir dotnetExePath (packages: Package list) =
    for pkg in packages do
        let fsProj, npmProj =
            let projPath = pkg.ProjectPath
            if projPath.EndsWith(".fsproj")
            then baseDir </> projPath, None
            else baseDir </> (projPath + ".fsproj"), Some (baseDir </> projPath </> "package.json")
        if File.Exists(fsProj) then
            pushNuget dotnetExePath (loadReleaseNotes fsProj) pkg fsProj
        else
            match npmProj with
            | Some npmProj ->
                if File.Exists(npmProj)
                then pushNpm (loadReleaseNotes npmProj) pkg.Build npmProj
                else failwithf "Couldn't find %s nor %s" fsProj npmProj
            | None ->
                failwithf "Couldn't find %s" fsProj

let publishPackages baseDir dotnetExePath packages =
    packages
    |> List.map Package
    |> publishPackages2 baseDir dotnetExePath

let githubRelease releaseNotesPath gitOwner project pushToGithub: unit =
    let release = ReleaseNotesHelper.LoadReleaseNotes releaseNotesPath
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "GitHub Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "GitHub Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + project))
        |> function
            | None -> "https://github.com/" + gitOwner + "/" + project
            | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    pushToGithub user pw release