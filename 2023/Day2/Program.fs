open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let maxRed = 12
let maxGreen = 13
let maxBlue = 14

let getMax (line: string) (color: string) =
    line.Split(color)
    |> Array.rev
    |> Array.tail
    |> Array.map (fun subString -> subString.Split(',', ';', ':') |> Array.last |> int)
    |> Array.max

let getAllMax (line: string) =
    getMax line "red", getMax line "green", getMax line "blue"

let part1Result =
    input
    |> Seq.map (fun line ->
        let id = (line.Split(":")[0]).Replace("Game ", "") |> int
        let red, green, blue = getAllMax line
        id, red, green, blue)
    |> Seq.filter (fun (_, red, green, blue) -> red <= maxRed && green <= maxGreen && blue <= maxBlue)
    |> Seq.map (fun (id, _, _, _) -> id)
    |> Seq.sum

printfn $"Part 1 result: {part1Result}"

let part2Result =
    input
    |> Seq.map (fun line ->
        let red, green, blue = getAllMax line
        red * green * blue)
    |> Seq.sum

printfn $"Part 2 result: {part2Result}"
