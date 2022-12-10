open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

type Point = int * int

let getPosition ((currentX, currentY): Point) (direction: string) (step: int) : Point =
    match direction with
    | "R" -> currentX + step, currentY
    | "D" -> currentX, currentY - step
    | "L" -> currentX - step, currentY
    | "U" -> currentX, currentY + step
    | _ -> failwith "lol"

let getNewPositions (current: Point) (motionMove: string) : Point array =
    let motionMove = motionMove.Split(" ")
    let direction = motionMove[0]
    let steps = int motionMove[1]

    [| 1..steps |]
    |> Array.map (getPosition current direction)

let isAdjacent ((aX, aY): Point) ((bX, bY): Point) : bool =
    [| (min aX bX) .. (max aX bX) |].Length <= 2
    && [| (min aY bY) .. (max aY bY) |].Length <= 2

let firstHeadPath: Point array =
    ([| (0, 0) |], input)
    ||> Array.fold (fun headPath motionMove ->
        let newHeadPositions =
            getNewPositions (headPath |> Array.last) motionMove

        Array.append headPath newHeadPositions)

let compare (a: int) (b: int) : int =
    match a - b with
    | 0 -> 0
    | res when res > 0 -> 1
    | _ -> -1

let initialTailPathState = [| 0, 0 |], (0, 0)

let getTailPath (headPath: Point array) =
    (initialTailPathState, headPath)
    ||> Array.fold
        (fun (tailPositionVisits: Point array, (tailX: int, tailY: int as tail: Point)) (headX: int, headY: int as head: Point) ->
            if isAdjacent head tail then
                tailPositionVisits, tail
            else
                let newTailPosition =
                    tailX + (compare headX tailX), tailY + (compare headY tailY)

                Array.append tailPositionVisits [| newTailPosition |], newTailPosition)
    |> fst

let part1Result =
    firstHeadPath
    |> getTailPath
    |> Array.distinct
    |> Array.length

let part2Result =
    (firstHeadPath, [| 1..9 |])
    ||> Array.fold (fun (headPath: Point array) _ ->
        getTailPath headPath)
    |> Array.distinct
    |> Array.length

printfn $"Part 1 result: {part1Result}"
printfn $"Part 2 result: {part2Result}"
