open System
open System.IO

type Symbol =
    { Value: char
      RowIndex: int
      ColumnIndex: int }

type NumberString =
    { Value: string
      RowIndex: int
      ColumnIndex: int
      AdjacentSymbol: Symbol option }

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let grid = input |> Array.map Seq.toArray

let toNumberStrings (rowIndex: int) (row: char array) : NumberString list =
    row
    |> Array.indexed
    |> Array.filter (fun (_, char) -> Char.IsDigit(char))
    |> Array.fold
        (fun numberStrings (columnIndex, digitChar) ->
            match numberStrings |> List.rev with
            | [] ->
                [ { Value = string digitChar
                    RowIndex = rowIndex
                    ColumnIndex = columnIndex
                    AdjacentSymbol = None } ]
            | previousNumberString :: tail ->
                let shouldAppendDigit =
                    previousNumberString.Value.Length + previousNumberString.ColumnIndex = columnIndex

                if shouldAppendDigit then
                    [ { previousNumberString with
                          Value = previousNumberString.Value + string digitChar } ]
                    |> List.append tail
                else
                    [ { Value = string digitChar
                        RowIndex = rowIndex
                        ColumnIndex = columnIndex
                        AdjacentSymbol = None } ]
                    |> List.append numberStrings)
        List.empty

let areValidIndices ((rowIndex, columnIndex): int * int) : bool =
    rowIndex >= 0
    && rowIndex < grid.Length
    && columnIndex >= 0
    && columnIndex < grid[0].Length

let hasAdjacentSymbol (numberString: NumberString) : bool =
    let firstColumnIndexToCheck = numberString.ColumnIndex - 1
    let lastColumnIndexToCheck = numberString.Value.Length + numberString.ColumnIndex

    [ firstColumnIndexToCheck..lastColumnIndexToCheck ]
    |> List.map (fun columnIndex ->
        [ numberString.RowIndex - 1, columnIndex ]
        |> List.append [ numberString.RowIndex + 1, columnIndex ])
    |> List.collect id
    |> List.append [ numberString.RowIndex, firstColumnIndexToCheck ]
    |> List.append [ numberString.RowIndex, lastColumnIndexToCheck ]
    |> List.filter areValidIndices
    |> List.exists (fun (rowIndex, columnIndex) ->
        let char = grid[rowIndex][columnIndex]
        not (Char.IsDigit(char)) && char <> '.')

let part1Result =
    grid
    |> Array.mapi toNumberStrings
    |> Array.map List.toArray
    |> Array.collect id
    |> Array.filter hasAdjacentSymbol
    |> Array.map (fun numberString -> int numberString.Value)
    |> Array.sum

printfn $"Part 1 result: {part1Result}"

let part2Result = 0

printfn $"Part 2 result: {part2Result}"
