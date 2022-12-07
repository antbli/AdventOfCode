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
    | MoveIn of directoryPath: string
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

let initFileTree =
    Map.empty
    |> Map.add
        "/"
        { Path = "/"
          Content = List.empty
          Size = 0 }

let fileTree, _ =
    ((initFileTree, initFileTree["/"]), input |> Array.tail)
    ||> Seq.fold (fun (fileTree: Map<string, Directory>, currentDirectory: Directory) (currentTerminalInfo: string) ->
        match getTerminalInfo currentTerminalInfo with
        | MoveOut -> fileTree, fileTree[(getParentPath currentDirectory.Path)]
        | MoveIn directoryPath ->
            let directoryPath =
                currentDirectory.Path + directoryPath + "/"

            if fileTree |> Map.containsKey directoryPath then
                fileTree, currentDirectory
            else
                let newDirectory =
                    { Path = directoryPath
                      Content = List.empty
                      Size = 0 }

                fileTree |> Map.add directoryPath newDirectory, newDirectory
        | ShowContent -> fileTree, currentDirectory
        | Content content ->
            let contentSize =
                match content with
                | File size -> size
                | Folder _ -> 0

            let updatedFileTree =
                fileTree
                |> Map.add
                    currentDirectory.Path
                    { Path = currentDirectory.Path
                      Content = currentDirectory.Content @ [ content ]
                      Size = currentDirectory.Size + contentSize }

            let updatedFileTree =
                updateSize updatedFileTree (getParentPath currentDirectory.Path) contentSize

            updatedFileTree, updatedFileTree[currentDirectory.Path])

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

// printfn $"Part 1 result: {part1Result}"
printfn $"Part 2 result: {part2Result}"
