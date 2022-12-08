open System.IO

let input =
    File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let treeGrid =
    input
    |> Array.map (fun i ->
        i.ToCharArray()
        |> Array.map string
        |> Array.map int)

let transposedTreeGrid =
    treeGrid |> Array.transpose

let isOuterTree ((xIndex, yIndex): int * int) : bool =
    xIndex = 0
    || yIndex = 0
    || yIndex = treeGrid.Length - 1
    || xIndex = transposedTreeGrid.Length - 1

let isVisibleWithTreesInFront (treesInFront: int array) (tree: int) : bool =
    treesInFront
    |> Array.exists (fun treeInFront -> treeInFront >= tree)
    |> not

let isVisible ((xIndex, yIndex): int * int) : bool =
    if isOuterTree (xIndex, yIndex) then
        true
    else
        let tree = treeGrid[yIndex][xIndex]

        let frontX, backX =
            treeGrid[yIndex] |> Array.splitAt xIndex

        let backX = backX |> Array.tail

        let frontY, backY =
            transposedTreeGrid[xIndex] |> Array.splitAt yIndex

        let backY = backY |> Array.tail

        let isVisibleFromLeft =
            isVisibleWithTreesInFront frontX tree

        let isVisibleFromRight =
            isVisibleWithTreesInFront backX tree

        let isVisibleFromTop =
            isVisibleWithTreesInFront frontY tree

        let isVisibleFromBottom =
            isVisibleWithTreesInFront backY tree

        isVisibleFromLeft
        || isVisibleFromRight
        || isVisibleFromTop
        || isVisibleFromBottom

let part1Result =
    treeGrid
    |> Array.mapi (fun yIndex xTrees ->
        xTrees
        |> Array.mapi (fun xIndex _ -> isVisible (xIndex, yIndex)))
    |> Array.concat
    |> Array.filter id
    |> Array.length

printfn $"Part 1 result: {part1Result}"
// printfn $"Part 2 result: {part2Result}"
