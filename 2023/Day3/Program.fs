open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let grid = input |> Array.map Seq.toArray

let toNumberStringsWithRowAndColumnIndex (rowIndex: int) (row: char array) : (string * int * int) list =
    row
    |> Array.indexed
    |> Array.filter (fun (_, char) -> Char.IsDigit(char))
    |> Array.fold
        (fun numberStringsWithRowAndColumnIndex (columnIndex, digitChar) ->
            match numberStringsWithRowAndColumnIndex |> List.rev with
            | [] -> List.singleton (string digitChar, rowIndex, columnIndex)
            | (previousNumberString, rowIndex, previousNumberStringColumnIndex) :: tail ->
                let shouldAppendDigit =
                    previousNumberString.Length + previousNumberStringColumnIndex = columnIndex

                if shouldAppendDigit then
                    List.append
                        tail
                        [ previousNumberString + string digitChar, rowIndex, previousNumberStringColumnIndex ]
                else
                    List.append numberStringsWithRowAndColumnIndex [ string digitChar, rowIndex, columnIndex ])
        List.empty<string * int * int>

let filterValidIndices ((rowIndex, columnIndex): int * int) : bool =
    rowIndex >= 0
    && rowIndex < grid.Length
    && columnIndex >= 0
    && columnIndex < grid[0].Length

let filterNumbersByAdjacentSymbol ((numberString, rowIndex, columnIndex): string * int * int) : bool =
    let firstColumnIndexToCheck = columnIndex - 1
    let lastColumnIndexToCheck = numberString.Length + columnIndex

    [ firstColumnIndexToCheck..lastColumnIndexToCheck ]
    |> List.map (fun columnIndex -> [ rowIndex - 1, columnIndex ] |> List.append [ rowIndex + 1, columnIndex ])
    |> List.collect id
    |> List.append [ rowIndex, firstColumnIndexToCheck ]
    |> List.append [ rowIndex, lastColumnIndexToCheck ]
    |> List.filter filterValidIndices
    |> List.exists (fun (rowIndex, columnIndex) ->
        let char = grid[rowIndex][columnIndex]
        not (Char.IsDigit(char)) && char <> '.')

let part1Result =
    grid
    |> Array.mapi toNumberStringsWithRowAndColumnIndex
    |> Array.map List.toArray
    |> Array.collect id
    |> Array.filter filterNumbersByAdjacentSymbol
    |> Array.map (fun (numberString, _, _) -> int numberString)
    |> Array.sum

printfn $"Part 1 result: {part1Result}"

let part2Result = 0

printfn $"Part 2 result: {part2Result}"
