open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let part1Result =
    input
    |> Seq.map Seq.toList
    |> Seq.map (fun chars ->
        match chars |> Seq.tryFind Char.IsDigit with
        | None -> 0
        | Some firstDigit ->
            let secondDigit = chars |> Seq.rev |> Seq.find Char.IsDigit
            $"{firstDigit}{secondDigit}" |> int)
    |> Seq.sum

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

let rec getFirstAndLastDigitValue
    (stringWithDigits: string)
    (firstDigitIndex: int)
    (firstDigitValue: int)
    (lastDigitIndex: int)
    (lastDigitValue: int)
    (digitTexts: string list)
    =
    match digitTexts with
    | [] -> firstDigitValue, lastDigitValue
    | digitText :: remainingDigitTexts ->
        let firstTextDigitIndex = stringWithDigits.IndexOf(digitText)

        if firstTextDigitIndex <> -1 then
            let firstDigitIndex, firstDigitValue =
                if firstTextDigitIndex < firstDigitIndex then
                    firstTextDigitIndex, digitTextToDigit[digitText]
                else
                    firstDigitIndex, firstDigitValue

            let lastTextDigitIndex = stringWithDigits.LastIndexOf(digitText)

            let lastDigitIndex, lastDigitValue =
                if lastTextDigitIndex > lastDigitIndex then
                    lastTextDigitIndex, digitTextToDigit[digitText]
                else
                    lastDigitIndex, lastDigitValue

            getFirstAndLastDigitValue
                stringWithDigits
                firstDigitIndex
                firstDigitValue
                lastDigitIndex
                lastDigitValue
                remainingDigitTexts
        else
            getFirstAndLastDigitValue
                stringWithDigits
                firstDigitIndex
                firstDigitValue
                lastDigitIndex
                lastDigitValue
                remainingDigitTexts

let part2Result =
    input
    |> Seq.map (fun stringWithDigits ->
        let chars = stringWithDigits |> Seq.toList

        let digitTexts = digitTextToDigit |> Map.keys |> Seq.toList

        match chars |> Seq.tryFindIndex Char.IsDigit with
        | Some firstDigitIndex ->
            let firstDigitValue = stringWithDigits.Substring(firstDigitIndex, 1) |> int

            let lastDigitIndex = chars |> Seq.findIndexBack Char.IsDigit

            let lastDigitValue = stringWithDigits.Substring(lastDigitIndex, 1) |> int

            getFirstAndLastDigitValue
                stringWithDigits
                firstDigitIndex
                firstDigitValue
                lastDigitIndex
                lastDigitValue
                digitTexts
        | None -> getFirstAndLastDigitValue stringWithDigits Int32.MaxValue 0 Int32.MinValue 0 digitTexts
        |> fun (first, last) -> $"{first}{last}" |> int)
    |> Seq.sum

printfn $"Part 2 result: {part2Result}"
