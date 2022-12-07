open System
open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

type DirectoryContent =
    | File of size: int
    | Folder of Path: string

type Directory =
    { Path: string
      Content: DirectoryContent list
      Size: int }

type TerminalInfo =
    | MoveOut
    | MoveIn of directoryName: string
    | ShowContent
    | Content of DirectoryContent

let getTerminalInfo (terminalInfo: string) : TerminalInfo =
    match terminalInfo with
    | t when t = "$ ls" -> ShowContent
    | t when t = "$ cd .." -> MoveOut
    | t when t.StartsWith("$ cd") -> MoveIn(t.Split(" ")[2])
    | t when t.StartsWith("dir") -> Content(Folder(t.Split(" ")[1]))
    | t when Char.IsDigit(t[0]) -> Content(File(int (t.Split(" ")[0])))
    | _ -> failwith "lol"

let getParentPath (path: string) : string =
    if path = "/" then
        ""
    else
        path.Substring(0, path.TrimEnd('/').LastIndexOf("/") + 1)

let rec updateDirectorySize (fileTree: Map<string, Directory>) (path: string) (size: int) : Map<string, Directory> =
    let directory = fileTree[path]

    fileTree
    |> Map.add
        directory.Path
        { Path = directory.Path
          Content = directory.Content
          Size = directory.Size + size }

let rec updateSize (fileTree: Map<string, Directory>) (path: string) (size: int) : Map<string, Directory> =
    match path with
    | "" -> fileTree
    | _ ->
        let updatedFileTree =
            updateDirectorySize fileTree path size

        updateSize updatedFileTree (getParentPath path) size

let addContent (fileTree: Map<string, Directory>) (path: string) (content: DirectoryContent) : Map<string, Directory> =
    let directory = fileTree[path]

    fileTree
    |> Map.add path { directory with Content = directory.Content @ [ content ] }

let createNewDirectory (path: string) : Directory =
    { Path = path
      Content = List.empty
      Size = 0 }

let getMoveInDirectoryPath (currentPath: string) (directoryName: string) : string =
    if directoryName = "/" then
        "/"
    else
        currentPath + directoryName + "/"

let initialFileTree: Map<string, Directory> =
    Map.empty |> Map.add "/" (createNewDirectory "/")

let fileTree =
    ((initialFileTree, "/"), input)
    ||> Seq.fold (fun (fileTree: Map<string, Directory>, currentPath: string) (currentTerminalInfo: string) ->
        match getTerminalInfo currentTerminalInfo with
        | MoveOut -> fileTree, getParentPath currentPath
        | MoveIn directoryName ->
            let directoryPath =
                getMoveInDirectoryPath currentPath directoryName

            if fileTree |> Map.containsKey directoryPath then
                fileTree, directoryPath
            else
                fileTree
                |> Map.add directoryPath (createNewDirectory directoryPath),
                directoryPath
        | ShowContent -> fileTree, currentPath
        | Content content ->
            match content with
            | File size ->
                let updatedFileTree =
                    addContent fileTree currentPath content

                (updateSize updatedFileTree currentPath size), currentPath
            | Folder _ -> (addContent fileTree currentPath content), currentPath)
    |> fst

let directorySizes =
    fileTree.Values
    |> Seq.map (fun directory -> directory.Size)

let part1Result =
    directorySizes
    |> Seq.where (fun directorySize -> directorySize <= 100000)
    |> Seq.sum

let totalSize = directorySizes |> Seq.max

let part2Result =
    directorySizes
    |> Seq.sort
    |> Seq.find (fun directorySize -> totalSize - directorySize < 40000000)

printfn $"Part 1 result: {part1Result}"
printfn $"Part 2 result: {part2Result}"
