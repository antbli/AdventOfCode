open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let cycles =
    input
    |> Array.map (fun instruction ->
        match instruction.Split(" ") with
        | [| _ |] -> [| 0 |]
        | [| _; value |] -> [| 0; int value |]
        | _ -> failwith "lol")
    |> Array.collect id

let cycleNumbersToCalculateSignalStrengthAt =
    [| 20; 60; 100; 140; 180; 220 |]

let part1Result =
    ((0, 1, 1), cycles)
    ||> Array.fold (fun (totalSignalStrength, xRegister, cycleNumber) valueToAddAfterCycle ->
        let signalStrength =
            cycleNumbersToCalculateSignalStrengthAt
            |> Array.tryFind (fun c -> c = cycleNumber)
            |> Option.map (fun c -> c * xRegister)
            |> Option.defaultValue 0

        totalSignalStrength + signalStrength, xRegister + valueToAddAfterCycle, cycleNumber + 1)
    |> (fun (signalStrength, _, _) -> signalStrength)
    
printfn $"Part 1 result: {part1Result}"

let getCrtPixel (spritePixelPositions: int array) (crtRow: string): string =
    spritePixelPositions
    |> Array.tryFind (fun p -> p = crtRow.Length)
    |> Option.map (fun _ -> "#")
    |> Option.defaultValue "."

let cycleNumbersToAddNewRowAt =
    [| 1; 41; 81; 121; 161; 201 |]
    
printf "Part 2 result:\n"

((Array.empty<string>, 1, 1), cycles)
||> Array.fold (fun (crt, xRegister, cycleNumber) valueToAddAfterCycle ->
    let spritePixelPositions = [| xRegister - 1.. xRegister + 1 |]
    
    let updatedCrt =
        if cycleNumbersToAddNewRowAt |> Array.contains cycleNumber then
            let crtPixel = getCrtPixel spritePixelPositions ""
            Array.append crt [|crtPixel|]
        else
            let currentCrtRow = crt |> Array.last
            let crtPixel = getCrtPixel spritePixelPositions currentCrtRow
            let updatedCrtRow = currentCrtRow + crtPixel
            crt |> Array.updateAt (crt.Length - 1) updatedCrtRow
    
    updatedCrt, xRegister + valueToAddAfterCycle, cycleNumber + 1)
|> (fun (crt, _, _) -> crt)
|> Array.iter (fun crtRow -> printfn $"{crtRow}")
