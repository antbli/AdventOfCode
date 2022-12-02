open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

type Move =
    | Rock
    | Paper
    | Scissors

type RoundResult =
    | Win
    | Draw
    | Loss

let getMoveScore (move: Move) : int =
    match move with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let getResultScore (result: RoundResult) : int =
    match result with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

let getRoundResult (opponentMove: Move) (myMove: Move) : RoundResult =
    if opponentMove = myMove then
        Draw
    else if opponentMove = Rock && myMove = Scissors then
        Loss
    else if opponentMove = Rock && myMove = Paper then
        Win
    else if opponentMove = Paper && myMove = Scissors then
        Win
    else if opponentMove = Paper && myMove = Rock then
        Loss
    else if opponentMove = Scissors && myMove = Rock then
        Win
    else
        Loss

let mapMove (move: string) : Move =
    match move with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith "Lol"

let mapToStrategy (opponentMove: Move) (myMove: Move) : Move =
    match myMove with
    | Rock ->
        match opponentMove with
        | Rock -> Scissors
        | Paper -> Rock
        | Scissors -> Paper
    | Paper -> opponentMove
    | Scissors ->
        match opponentMove with
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock

let getRoundMoves (moves: string) : Move * Move =
    let moves = moves.Split(" ")
    mapMove moves[0], mapMove moves[1]

let result (useStrategy: bool) =
    input
    |> Seq.map getRoundMoves
    |> Seq.fold
        (fun sum (opponentMove, myMove) ->
            let myMove =
                if useStrategy then
                    mapToStrategy opponentMove myMove
                else
                    myMove

            let roundResult =
                getRoundResult opponentMove myMove

            let resultScore = getResultScore roundResult
            let moveScore = getMoveScore myMove

            sum + resultScore + moveScore)
        0

printfn $"Part 1 result: {result false}"
printfn $"Part 2 result: {result true}"
