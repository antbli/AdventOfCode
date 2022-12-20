open System
open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

type Point = int * int

type Letter = char

type Distance = int

let heightMapWithStartAndEndLetters: (Point * Letter) array =
    input
    |> Array.mapi (fun yPosition letters ->
        letters.ToCharArray()
        |> Array.mapi (fun xPosition letter -> (xPosition, yPosition), letter))
    |> Array.collect id

let getPositionOfLetter (letterToFind: Letter) : Point =
    heightMapWithStartAndEndLetters
    |> Array.find (fun (_, letter) -> letter = letterToFind)
    |> fst

let startPosition: Point =
    getPositionOfLetter 'S'

let endPosition: Point =
    getPositionOfLetter 'E'

let heightMap: Map<Point, Letter> =
    heightMapWithStartAndEndLetters
    |> Array.map (fun (position, letter) ->
        let letter =
            match letter with
            | 'S' -> 'a'
            | 'E' -> 'z'
            | _ -> letter

        position, letter)
    |> Map.ofArray

let isValidNeighbourLetters (letterA: Letter) (letterB: Letter) : bool =
    let letterAInt = int letterA
    let letterBInt = int letterB

    letterAInt = letterBInt
    || letterAInt - 1 = letterBInt
    || letterAInt + 1 = letterBInt

let getNeighbours (xPosition, yPosition as position: Point) (validPositions: Point list) : Point list =
    let leftPosition = xPosition - 1, yPosition
    let rightPosition = xPosition + 1, yPosition
    let upPosition = xPosition, yPosition - 1
    let downPosition = xPosition, yPosition + 1

    let letter = heightMap[position]

    let isValidNeighbour (neighbourPosition: Point) : bool =
        (validPositions |> List.contains neighbourPosition)
        && isValidNeighbourLetters letter heightMap[neighbourPosition]

    [ leftPosition
      rightPosition
      upPosition
      downPosition ]
    |> List.filter isValidNeighbour

let rec createShortestPathData
    (shortestPathData: Map<Point, Distance * Point option>)
    (unvisitedPositions: Point list)
    : Map<Point, Distance * Point option> =
    match unvisitedPositions with
    | [] -> shortestPathData
    | currentPosition :: unvisitedPositions ->
        let unvisitedPositionsWithShortestDistance =
            unvisitedPositions
            |> List.map (fun position -> position, fst shortestPathData[position])

        let neighbourPositions =
            getNeighbours currentPosition unvisitedPositions

        let currentPositionShortestDistance, _ =
            shortestPathData[currentPosition]

        let updatedShortestPathData, updatedUnvisitedPositionsWithShortestDistance =
            ((shortestPathData, unvisitedPositionsWithShortestDistance), neighbourPositions)
            ||> List.fold (fun (shortestPathData, unvisitedPositionsWithShortestDistance) neighbourPosition ->
                let shortestDistance, _ =
                    shortestPathData[neighbourPosition]

                let calculatedDistance =
                    currentPositionShortestDistance + 1

                if calculatedDistance < shortestDistance then
                    let updatedShortestPathData =
                        shortestPathData
                        |> Map.add neighbourPosition (calculatedDistance, Some currentPosition)

                    let indexWhereToInsertNewShortestDistance =
                        unvisitedPositionsWithShortestDistance
                        |> List.findIndex (fun (_, shortestDistance) -> shortestDistance > calculatedDistance)

                    let updatedUnvisitedPositionsWithShortestDistance =
                        unvisitedPositionsWithShortestDistance
                        |> List.insertAt indexWhereToInsertNewShortestDistance (neighbourPosition, calculatedDistance)

                    updatedShortestPathData, updatedUnvisitedPositionsWithShortestDistance
                else
                    shortestPathData, unvisitedPositionsWithShortestDistance)

        let updatedUnvisitedPositions =
            updatedUnvisitedPositionsWithShortestDistance
            |> List.map fst
            |> List.distinct

        createShortestPathData updatedShortestPathData updatedUnvisitedPositions

let getShortestPathData (startPosition: Point) : Map<Point, Point> =
    let unvisitedPositions =
        heightMap
        |> Map.remove startPosition
        |> Map.toList
        |> List.map fst
        |> List.append [ startPosition ]

    let initialShortestPathData: Map<Point, Distance * Point option> =
        heightMap
        |> Map.map (fun _ _ -> Int32.MaxValue, None)
        |> Map.add startPosition (0, None)

    createShortestPathData initialShortestPathData unvisitedPositions
    |> Map.toList
    |> List.choose (fun (position, (_, previousPosition)) ->
        match previousPosition with
        | Some previousPosition -> Some (position, previousPosition)
        | None -> None)
    |> Map.ofList

let rec calculateFewestSteps (startPosition: Point) (nextPosition: Point) (shortestPathData: Map<Point, Point>) : int =
    if nextPosition = startPosition then
        0
    else
        (calculateFewestSteps startPosition shortestPathData[nextPosition] shortestPathData) + 1

printfn $"Part 1 result: {calculateFewestSteps startPosition endPosition (getShortestPathData startPosition)}"
// printfn $"Part 2 result: {part2Result[0] * part2Result[1]}"
