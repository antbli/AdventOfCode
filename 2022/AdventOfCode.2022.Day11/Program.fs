open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input1.txt"))

type MonkeyTurn =
    { Items: int array
      Operation: string
      DivisibleBy: int
      IsDivisibleMonkey: int
      IsNotDivisibleMonkey: int
      InspectedItems: int }

let calculateWorryLevel (item: int) (operation: string) : int =
    let operatorAndValue = operation.Split(" ")

    let value =
        if operatorAndValue[1] = "old" then
            item
        else
            int operatorAndValue[1]

    match operatorAndValue[0] with
    | "*" -> value * item
    | "+" -> value + item
    | _ -> failwith "lol"

let monkeyIndexes =
    input
    |> Array.indexed
    |> Array.filter (fun (_, note) -> note.StartsWith("Monkey"))
    |> Array.map fst

let firstRound =
    monkeyIndexes
    |> Array.map (fun index ->
        let startingItems =
            (input[ index + 1 ].Split(":")[1]).Split(",")
            |> Array.map int

        let operation =
            input[ index + 2 ].Split("old ")[1]

        let divisibleBy =
            int (input[ index + 3 ].Split("by ")[1])

        let trueMonkey =
            int (input[ index + 4 ].Split("monkey ")[1])

        let falseMonkey =
            int (input[ index + 5 ].Split("monkey ")[1])

        { Items = startingItems
          Operation = operation
          DivisibleBy = divisibleBy
          IsDivisibleMonkey = trueMonkey
          IsNotDivisibleMonkey = falseMonkey
          InspectedItems = 0 })

let runRound (round: MonkeyTurn array) =
    (round, [| 0 .. round.Length - 1 |])
    ||> Array.fold (fun round monkey ->
        let monkeyTurn = round[monkey]

        (round, monkeyTurn.Items)
        ||> Array.fold (fun round item ->
            let worryLevel = (calculateWorryLevel item monkeyTurn.Operation) / 3

            let monkeyToThrowTo =
                if worryLevel % monkeyTurn.DivisibleBy = 0 then
                    monkeyTurn.IsDivisibleMonkey
                else
                    monkeyTurn.IsNotDivisibleMonkey

            let monkeyTurnToUpdate =
                round[monkeyToThrowTo]

            let updatedMonkeyTurn =
                { monkeyTurnToUpdate with Items = Array.append monkeyTurnToUpdate.Items [| worryLevel |] }

            round
            |> Array.updateAt monkeyToThrowTo updatedMonkeyTurn)
        |> Array.updateAt
            monkey
            { monkeyTurn with
                Items = [||]
                InspectedItems =
                    monkeyTurn.InspectedItems
                    + monkeyTurn.Items.Length })


let part1Result =
    (firstRound, [| 1..20 |])
    ||> Array.fold (fun (round: MonkeyTurn array) _ -> runRound round)
    |> Array.map (fun monkeyTurn -> monkeyTurn.InspectedItems)
    |> Array.sortDescending
    |> Array.take 2

printfn $"Part 1 result: {part1Result[0] * part1Result[1]}"
// printfn $"Part 2 result: {part2Result[0] * part2Result[1]}"
