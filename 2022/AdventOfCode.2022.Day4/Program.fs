open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let toSeq (assignments: string) : int seq =
    let assignments = assignments.Split('-')
    seq { int assignments[0] .. int assignments[1] }

let toPairSeq (assignmentPairs: string) : int seq * int seq =
    let pairs = assignmentPairs.Split(',')
    toSeq pairs[0], toSeq pairs[1]

let rangeFullyContains (first: int seq) (second: int seq) : bool =
    first
    |> Seq.forall (fun lol -> second |> Seq.contains lol)

let anyRangeFullyContains (first: int seq, second: int seq) : bool =
    rangeFullyContains first second
    || rangeFullyContains second first

let rangeOverlaps (first: int seq) (second: int seq) : bool =
    first
    |> Seq.forall (fun item -> second |> Seq.contains item)

let anyRangeOverlaps (first: int seq, second: int seq) : bool =
    first
    |> Seq.exists (fun item -> second |> Seq.contains item)

let part1Result =
    input
    |> Array.map toPairSeq
    |> Array.filter anyRangeFullyContains
    |> Array.length

let part2Result =
    input
    |> Array.map toPairSeq
    |> Array.filter anyRangeOverlaps
    |> Array.length

printfn $"Part 1 result: {part1Result}"
printfn $"Part 2 result: {part2Result}"
