open System
open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

type DirectoryContent =
    | File of size: int
    | Directory of name: string

type Directory =
    { Name: string
      Content: DirectoryContent list
      ParentDirectoryName: string
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
    | t when t.StartsWith("dir") -> Content(Directory(t.Split(" ")[1]))
    | t when Char.IsDigit(t[0]) -> Content(File(int (t.Split(" ")[0])))
    | _ -> failwith "lol"

let initFileTree =
    Map.empty
    |> Map.add
        "root"
        { Name = "root"
          Content = [ Directory "/" ]
          ParentDirectoryName = "lol nothing"
          Size = 0 }

let rec updateParentSize
    (fileTree: Map<string, Directory>)
    (parentDirectoryName: string)
    (size: int)
    : Map<string, Directory> =
    if parentDirectoryName = "lol nothing" then
        fileTree
    else
        let parent = fileTree[parentDirectoryName]

        let updatedFileTree =
            fileTree
            |> Map.add
                parentDirectoryName
                { Name = parent.Name
                  Content = parent.Content
                  ParentDirectoryName = parent.ParentDirectoryName
                  Size = parent.Size + size }

        updateParentSize updatedFileTree parent.ParentDirectoryName size

let fileTree, _ =
    ((initFileTree, "root"), input)
    ||> Seq.fold
        (fun (fileTree: Map<string, Directory>, currentDirectoryName: string) (currentTerminalInfo: string) ->
            let terminalInfo =
                getTerminalInfo currentTerminalInfo

            match terminalInfo with
            | MoveOut ->
                fileTree,
                if currentDirectoryName = "root" then
                    "root"
                else
                    fileTree[currentDirectoryName].ParentDirectoryName
            | MoveIn directoryName ->
                if fileTree |> Map.containsKey directoryName then
                    fileTree, directoryName
                else
                    fileTree
                    |> Map.add
                        directoryName
                        { Name = directoryName
                          Content = List.empty
                          ParentDirectoryName = currentDirectoryName
                          Size = 0 },
                    directoryName
            | ShowContent -> fileTree, currentDirectoryName
            | Content content ->
                let currentDirectory =
                    fileTree[currentDirectoryName]

                let contentSize =
                    match content with
                    | File size -> size
                    | Directory _ -> 0

                let updatedFileTree =
                    fileTree
                    |> Map.add
                        currentDirectoryName
                        { Name = currentDirectory.Name
                          Content = currentDirectory.Content @ [ content ]
                          ParentDirectoryName = currentDirectory.ParentDirectoryName
                          Size = currentDirectory.Size + contentSize }
                        
                let updatedFileTree = updateParentSize updatedFileTree currentDirectory.ParentDirectoryName contentSize

                updatedFileTree, currentDirectoryName)

let res =
    fileTree.Values
    |> Seq.map (fun directory -> directory.Size)
    |> Seq.where (fun a -> a <= 100000)
    |> Seq.sum

printfn $"Part 1 result: {res}"
// printfn $"Part 2 result: {findMarkerPos 14 14}"
