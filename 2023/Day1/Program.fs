open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let combineEachLinesFirstAndLastDigitAndSum (input: string seq) =
    input
    |> Seq.map Seq.toList
    |> Seq.map (fun chars ->
        match chars |> Seq.tryFind Char.IsDigit with
        | None -> 0
        | Some firstDigit ->
            let secondDigit = chars |> Seq.rev |> Seq.find Char.IsDigit
            $"{firstDigit}{secondDigit}" |> int)
    |> Seq.sum

let part1Result = combineEachLinesFirstAndLastDigitAndSum input

printfn $"Part 1 result: {part1Result}"

let digitTextToDigit =
    [ "one", 1
      "two", 2
      "three", 3
      "four", 4
      "five", 5
      "six", 6
      "seven", 7
      "eight", 8
      "nine", 9 ]
    |> Map.ofList

let rec replaceDigitTexts (line: string) (digitTexts: string list) =
    match digitTexts with
    | [] -> line
    | digitText :: remainingDigitTexts ->
        replaceDigitTexts
            (line.Replace(digitText, $"{digitText}{digitTextToDigit[digitText]}{digitText}"))
            remainingDigitTexts

let part2Result =
    input
    |> Seq.map (fun line -> replaceDigitTexts line (digitTextToDigit |> Map.keys |> Seq.toList))
    |> combineEachLinesFirstAndLastDigitAndSum

printfn $"Part 2 result: {part2Result}"
