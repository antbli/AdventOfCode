open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let getScratchcardNumbers (scratchcard: string) : int array * int array =
    (scratchcard.Split(':')[1]).Split('|')
    |> Array.map (_.Split(' ', StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun numbers -> numbers |> Array.map int)
    |> fun numbers -> numbers[0], numbers[1]

let filterWinningNumbers (myNumbers: int array, winningNumbers: int array) : int array =
    myNumbers
    |> Array.filter (fun myNumber -> winningNumbers |> Array.contains myNumber)

let getWinningNumbers (scratchcard: string) : int array =
    getScratchcardNumbers scratchcard |> filterWinningNumbers

let rec calculatePoints (count: int) (matches: int list) : int =
    match matches with
    | [] -> count
    | _ :: tail ->
        if count = 0 then
            calculatePoints 1 tail
        else
            calculatePoints (count + count) tail

let part1Result =
    input
    |> Array.map (fun scratchcard -> scratchcard |> getWinningNumbers |> Array.toList |> calculatePoints 0)
    |> Array.sum

printfn $"Part 1 result: {part1Result}"

let updateScratchcardCopiesList
    (currentScratchcardCopies: int)
    (scratchcardCopiesList: int list)
    (index: int)
    : int list =
    try
        scratchcardCopiesList
        |> List.updateAt index (scratchcardCopiesList[index] + currentScratchcardCopies + 1)
    with _ ->
        List.append scratchcardCopiesList [ currentScratchcardCopies + 1 ]

let part2Result =
    ((0, List.empty<int>), input)
    ||> Array.fold (fun (totalScratchcards, scratchcardCopiesList) scratchcard ->
        let winningNumberIndices =
            getWinningNumbers scratchcard |> Array.indexed |> Array.map fst |> Array.toList

        let currentScratchcards, updatedScratchcardCopies =
            match scratchcardCopiesList with
            | [] -> 1, winningNumberIndices |> List.map (fun _ -> 1)
            | currentScratchcardCopies :: upcomingScratchcardCopiesList ->
                let updatedScratchcardCopiesList =
                    (upcomingScratchcardCopiesList, winningNumberIndices)
                    ||> List.fold (updateScratchcardCopiesList currentScratchcardCopies)

                currentScratchcardCopies + 1, updatedScratchcardCopiesList

        totalScratchcards + currentScratchcards, updatedScratchcardCopies)
    |> fst

printfn $"Part 2 result: {part2Result}"
