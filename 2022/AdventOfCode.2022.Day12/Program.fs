open System
open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input1.txt"))

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

let rec createShortestPathData (shortestPathData: Map<Point, Distance * Point option>) (unvisitedPositions: Point list) =
    match unvisitedPositions with
    | [] -> shortestPathData
    | currentPosition :: remainingUnvisitedPositions ->
        let neighbourPositions =
            getNeighbours currentPosition remainingUnvisitedPositions

        let currentShortestDistance, _ = shortestPathData[currentPosition]
        
        let updatedShortestPathData, updatedUnvisitedPositions =
            ((shortestPathData, remainingUnvisitedPositions), neighbourPositions)
            ||> List.fold (fun (shortestPathData, unvisitedPositions) neighbourPosition ->
                let shortestDistance, previousPosition = shortestPathData[neighbourPosition]
                
                let calculatedDistance = currentShortestDistance + 1
                
                let newShortestDistance =
                    if calculatedDistance < shortestDistance then
                        calculatedDistance
                    else
                        shortestDistance
                
                shortestPathData, unvisitedPositions)
            
        createShortestPathData updatedShortestPathData updatedUnvisitedPositions
        


let getShortestPathCalculationData (startPosition: Point) : Map<Point, Distance * Point option> =
    let unvisited =
        heightMap
        |> Map.remove startPosition
        |> Map.map (fun _ _ -> Int32.MaxValue)
        |> Map.toArray
        |> Array.append [| startPosition, 0 |]

    Map.empty

// let getShortestPathCalculationData (startPosition: Point) : Map<Point, Distance * Point option> =
//     let initialShortestPathCalculationData =
//         Map.empty<Point, Distance * Point option>
//         |> Map.add startPosition (0, None)
//
//     let unvisited =
//         heightMap
//         |> Map.remove startPosition
//         |> Map.map (fun _ _ -> Int32.MaxValue)
//         |> Map.toArray
//         |> Array.append [| startPosition, 0 |]
//
//     ((initialShortestPathCalculationData, unvisited), [| 1 .. heightMap.Count |])
//     ||> Array.fold (fun (shortestPathCalculationData, unvisited) _ ->
//         let currentPosition, currentDistance =
//             unvisited |> Array.head
//
//         printf $"{DateTime.Now}, Unvisited: {unvisited.Length}\n"
//
//         let updatedUnvisited =
//             unvisited
//             |> Array.tail
//
//         let neighbours =
//             getUnvisitedNeighbours currentPosition (updatedUnvisited |> Array.map fst)
//
//         let updatedShortestPathCalculationData, updatedUnvisited =
//             ((shortestPathCalculationData, updatedUnvisited), neighbours)
//             ||> Array.fold (fun (shortestPathCalculationData, unvisited) neighbour ->
//                 let currentNeighbourData =
//                     shortestPathCalculationData
//                     |> Map.tryFind neighbour
//
//                 let updatedData =
//                     match currentNeighbourData with
//                     | Some (distance, previousPosition) ->
//                         if currentDistance + 1 < distance then
//                             currentDistance + 1, Some currentPosition
//                         else
//                             distance, previousPosition
//                     | None -> currentDistance + 1, Some currentPosition
//
//                 let updatedUnvisited =
//                     unvisited
//                     |> Array.removeAt (unvisited |> Array.findIndex (fun (position, _) -> position = neighbour))
//
//                 let newUnvisitedIndex =
//                     updatedUnvisited
//                     |> Array.findIndex (fun (_, distance) -> distance > fst updatedData)
//
//                 let updatedUnvisited =
//                     updatedUnvisited
//                     |> Array.insertAt newUnvisitedIndex (neighbour, fst updatedData)
//
//                 shortestPathCalculationData
//                 |> Map.add neighbour updatedData, updatedUnvisited)
//
//         updatedShortestPathCalculationData, updatedUnvisited)
//     |> fst
//
// let shortestPathCalculationData = getShortestPathCalculationData startPosition
//
// let rec calculateFewestSteps
//     (next: Point option)
//     : int =
//     match next with
//     | None -> 0
//     | Some position ->
//         let next = snd shortestPathCalculationData[position]
//         (calculateFewestSteps next) + 1

// printfn $"Part 1 result: {calculateFewestSteps (Some endPosition)}"
// printfn $"Part 2 result: {part2Result[0] * part2Result[1]}"
