open System
open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let rec findCommonItem (rucksacks: char array array) : char =
    let filteredHead =
        rucksacks[0]
        |> Array.filter (fun item -> rucksacks[1] |> Array.contains item)
        |> Array.distinct

    if filteredHead.Length = 1 then
        filteredHead[0]
    else
        findCommonItem (Array.append [| filteredHead |] (rucksacks |> Array.removeManyAt 0 2))

let getItemPriority (item: char) : int =
    if Char.IsUpper(item) then
        int item - 38
    else
        int (Char.ToUpper(item)) - 64

let part1Result =
    input
    |> Array.map (fun rucksack -> rucksack.ToCharArray() |> Array.splitInto 2)
    |> Array.map findCommonItem
    |> Array.map getItemPriority
    |> Array.sum

let part2Result =
    input
    |> Array.map (fun rucksack -> rucksack.ToCharArray())
    |> Array.splitInto (input.Length / 3)
    |> Array.map findCommonItem
    |> Array.map getItemPriority
    |> Array.sum

printfn $"Part 1 result: {part1Result}"
printfn $"Part 2 result: {part2Result}"
