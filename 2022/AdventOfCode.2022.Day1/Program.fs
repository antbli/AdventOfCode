open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let calorieSums =
    (seq { 0 }, input)
    ||> Seq.fold (fun (calories: int seq) (currentValue: string) ->
        if currentValue.Trim().Length = 0 then
            Seq.append (seq { 0 }) calories
        else
            let currentSum =
                Seq.head calories + int currentValue

            Seq.append (seq { currentSum }) (Seq.tail calories))
    |> Seq.sortDescending

let part1Result = calorieSums |> Seq.head

let part2Result =
    calorieSums |> Seq.take 3 |> Seq.sum

printfn $"Part 2 result: {part1Result}"
printfn $"Part 1 result: {part2Result}"
