open System
open System.IO

let input =
    File.ReadAllText(Path.Join(__SOURCE_DIRECTORY__, "input.txt")).Split("\n\n")

let initialStacksInput =
    input[0].Split("\n") |> Array.rev

let getStackNrIndex (stackNr: int) : int =
    initialStacksInput[0].IndexOf(stackNr.ToString())

let fillStack (stackIndex: int) : char array =
    initialStacksInput
    |> Array.tail
    |> Array.map (fun stackRow -> stackRow[stackIndex])

let filterEmptySlots (crates: char array) : char array =
    crates |> Array.filter (fun crate -> crate <> ' ')

let initialStacks =
    [| 1..9 |]
    |> Array.map getStackNrIndex
    |> Array.map fillStack
    |> Array.map filterEmptySlots

let extractProcedureActions (procedureItem: string) : int array =
    procedureItem.Split(' ')
    |> Array.filter (fun s -> fst (Int32.TryParse(s)))
    |> Array.map int

let rearrangementProcedure =
    input[1].Split("\n")
    |> Array.map extractProcedureActions

let rearrange (useCrateMover9001: bool) (stacks: char array array) (procedureItem: int array) : char array array =
    let nrOfCratesToMove = procedureItem[0]
    let stackToMoveFrom = procedureItem[1] - 1
    let stackToMoveTo = procedureItem[2] - 1

    let cratesToMove =
        stacks[stackToMoveFrom]
        |> Array.rev
        |> Array.take nrOfCratesToMove

    let cratesToMove =
        if useCrateMover9001 then
            cratesToMove |> Array.rev
        else
            cratesToMove

    let updatedMoveToStack =
        Array.append stacks[stackToMoveTo] cratesToMove

    let updatedMoveFromStack =
        stacks[stackToMoveFrom]
        |> Array.rev
        |> Array.skip nrOfCratesToMove
        |> Array.rev

    stacks
    |> Array.updateAt stackToMoveTo updatedMoveToStack
    |> Array.updateAt stackToMoveFrom updatedMoveFromStack

let part1Result =
    (initialStacks, rearrangementProcedure)
    ||> Array.fold (rearrange false)
    |> Array.map Array.rev
    |> Array.map Array.head
    |> String

let part2Result =
    (initialStacks, rearrangementProcedure)
    ||> Array.fold (rearrange true)
    |> Array.map Array.rev
    |> Array.map Array.head
    |> String

printfn $"Part 1 result: {part1Result}"
printfn $"Part 2 result: {part2Result}"
