open System.IO

let input =
    File.ReadAllText(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))
let rec findMarkerPos (maybeMarkerPos: int) (charLength: int) : int =
    let maybeMarker = input.Substring(maybeMarkerPos - charLength, charLength)
    
    let uniqueCharLength =
        maybeMarker.ToCharArray()
        |> Array.distinct
        |> Array.length
    
    if uniqueCharLength = charLength then
        maybeMarkerPos
    else
        findMarkerPos (maybeMarkerPos + 1) charLength

printfn $"Part 1 result: {findMarkerPos 4 4}"
printfn $"Part 2 result: {findMarkerPos 14 14}"
