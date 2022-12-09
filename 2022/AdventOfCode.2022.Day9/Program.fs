open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input1.txt"))

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

let part1Result =
    ((Set.empty |> Set.add (0, 0), (0, 0), (0, 0)), input)
    ||> Array.fold
        (fun (tailPositionVisits: Set<Point>, currentHead: Point, currentTail: Point) (motionMove: string) ->
            let newHeadPositions =
                getNewPositions currentHead motionMove

            let newCurrentHead =
                newHeadPositions |> Array.last

            if isAdjacent newCurrentHead currentTail then
                tailPositionVisits, newCurrentHead, currentTail
            else
                let newTailPositionVisits =
                    newHeadPositions
                    |> Array.rev
                    |> Array.tail

                let updatedTailPositionVisits =
                    (tailPositionVisits, newTailPositionVisits)
                    ||> Array.fold (fun tailPositionVisits tailPosition -> tailPositionVisits |> Set.add tailPosition)

                updatedTailPositionVisits, newCurrentHead, newHeadPositions |> Array.head)
    |> (fun (tailPositionVisits, _, _) -> tailPositionVisits)
    |> Set.count

printfn $"Part 1 result: {part1Result}"
// printfn $"Part 2 result: {part2Result}"
