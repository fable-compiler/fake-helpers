module Fable.FakeHelpers

open System
open System.IO
open System.Text.RegularExpressions
open Fake
open Fake.ReleaseNotesHelper

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

let needsPublishing (versionRegex: Regex) (releaseNotes: ReleaseNotes) projFile =
    printfn "Project: %s" projFile
    if releaseNotes.NugetVersion.ToUpper().EndsWith("NEXT")
    then
        printfn "Version in Release Notes ends with NEXT, don't publish yet."
        false
    else
        File.ReadLines(projFile)
        |> Seq.tryPick (fun line ->
            let m = versionRegex.Match(line)
            if m.Success then Some m else None)
        |> function
            | None -> failwith "Couldn't find version in project file"
            | Some m ->
                let sameVersion = m.Groups.[1].Value = releaseNotes.NugetVersion
                if sameVersion then
                    printfn "Already version %s, no need to publish." releaseNotes.NugetVersion
                not sameVersion

let pushNuget dotnetExePath (releaseNotes: ReleaseNotes) (projFiles: string list) =
    let versionRegex = Regex("<Version>(.*?)</Version>", RegexOptions.IgnoreCase)
    projFiles
    |> Seq.map (fun projFile -> __SOURCE_DIRECTORY__ </> projFile)
    |> Seq.filter (needsPublishing versionRegex releaseNotes)
    |> Seq.iter (fun projFile ->
        let projDir = Path.GetDirectoryName(projFile)
        let nugetKey =
            match environVarOrNone "NUGET_KEY" with
            | Some nugetKey -> nugetKey
            | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"
        // Restore dependencies here so they're updated to latest project versions
        run projDir dotnetExePath "restore"
        // Update the project file
        (versionRegex, projFile) ||> replaceLines (fun line _ ->
            versionRegex.Replace(line, "<Version>"+releaseNotes.NugetVersion+"</Version>") |> Some)
        try
            let tempDir = projDir </> "temp"
            CleanDir tempDir
            run projDir dotnetExePath ("pack -c Release -o " + tempDir)
            let pushCmd =
                let nupkg = Directory.GetFiles(tempDir) |> Seq.head
                sprintf "nuget push %s -s nuget.org -k %s" nupkg nugetKey
            run projDir dotnetExePath pushCmd
            CleanDir tempDir
        with _ ->
            Path.GetFileNameWithoutExtension(projFile)
            |> printfn "There's been an error when pushing project: %s"
            printfn "Please revert the version change in .fsproj"
            reraise()
    )

let publishPackages dotnetExePath packages =
    for pkg in packages do
        let projFile = __SOURCE_DIRECTORY__ </> (pkg + ".fsproj")
        let projDir = Path.GetDirectoryName(projFile)
        let release =
            findFileUpwards "RELEASE_NOTES.md" projDir
            |> ReleaseNotesHelper.LoadReleaseNotes
        pushNuget dotnetExePath release [projFile]
