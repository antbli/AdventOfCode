open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let grid = input |> Array.map Seq.toArray

let part1Result =
    grid
    |> Array.mapi (fun rowIndex row ->
        row
        |> Array.mapi (fun columnIndex char ->
            if Char.IsDigit(char) then
                Some(string char, columnIndex)
            else
                None)
        |> Array.choose id
        |> Array.fold
            (fun numberStringsWithRowAndColumnIndex (digitString, columnIndex) ->
                match numberStringsWithRowAndColumnIndex |> List.rev with
                | [] -> List.singleton (digitString, rowIndex, columnIndex)
                | (previousNumberString, rowIndex, previousNumberStringColumnIndex) :: tail ->
                    if (previousNumberString |> String.length) + previousNumberStringColumnIndex = columnIndex then
                        (previousNumberString + digitString, rowIndex, previousNumberStringColumnIndex)
                        :: tail
                        |> List.rev
                    else
                        List.append numberStringsWithRowAndColumnIndex [ digitString, rowIndex, columnIndex ])
            List.empty<string * int * int>)
    |> Array.map List.toArray
    |> Array.collect id
    |> Array.filter (fun (numberString, rowIndex, columnIndex) ->
        let firstColumnIndexToCheck = columnIndex - 1
        let lastColumnIndexToCheck = (numberString |> String.length) + columnIndex

        [ firstColumnIndexToCheck..lastColumnIndexToCheck ]
        |> List.map (fun columnIndex -> [ rowIndex - 1, columnIndex ] |> List.append [ rowIndex + 1, columnIndex ])
        |> List.collect id
        |> List.append [ rowIndex, firstColumnIndexToCheck ]
        |> List.append [ rowIndex, lastColumnIndexToCheck ]
        |> List.filter (fun (rowIndex, columnIndex) ->
            rowIndex >= 0
            && rowIndex < grid.Length
            && columnIndex >= 0
            && columnIndex < grid[0].Length)
        |> List.exists (fun (rowIndex, columnIndex) ->
            let char = grid[rowIndex][columnIndex]

            not (Char.IsDigit(char)) && char <> '.'

            ))
    |> Array.map (fun (numberString, _, _) -> int numberString)
    |> Array.sum

printfn $"Part 1 result: {part1Result}"

let part2Result = 0

printfn $"Part 2 result: {part2Result}"
