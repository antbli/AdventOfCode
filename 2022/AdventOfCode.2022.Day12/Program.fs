open System
open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

type Point = int * int

type Letter = char

type Distance = int

let initialHeightMapData: (Point * Letter) array =
    input
    |> Array.mapi (fun x s ->
        s.ToCharArray()
        |> Array.mapi (fun y c -> (x, y), c))
    |> Array.collect id

let getPositionOfLetter (letterToFind: Letter) : Point =
    initialHeightMapData
    |> Array.find (fun (_, letter) -> letter = letterToFind)
    |> fst

let startPositionLetter = 'S'
let endPositionLetter = 'E'

let startPosition: Point = getPositionOfLetter 'S'

let endPosition: Point = getPositionOfLetter 'E'

let getLetter (letter: Letter) =
    match letter with
    | 'S' -> 'a'
    | 'E' -> 'z'
    | _ -> letter

let heightMapData: (Point * Letter * Distance) array =
    initialHeightMapData
    |> Array.map (fun (position, letter) -> position, getLetter letter, Int32.MaxValue)
    
let 

printfn $"Part 1 result: a"
// printfn $"Part 2 result: {part2Result[0] * part2Result[1]}"
