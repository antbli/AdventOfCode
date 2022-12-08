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

let getSurroundingTrees ((xIndex, yIndex): int * int) : int array * int array * int array * int array =
    let frontX, backX =
        treeGrid[yIndex] |> Array.splitAt xIndex

    let backX = backX |> Array.tail

    let frontY, backY =
        transposedTreeGrid[xIndex] |> Array.splitAt yIndex

    let backY = backY |> Array.tail

    frontX, backX, frontY, backY

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

        let frontX, backX, frontY, backY =
            getSurroundingTrees (xIndex, yIndex)

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

let getVisibleTreesInFront (treesInFront: int array) (tree: int) : int =
    match treesInFront with
    | [||] -> 0
    | treesInFront ->
        let visibleTreesInFrontIndex =
            treesInFront
            |> Array.tryFindIndex (fun treeInFront -> treeInFront >= tree)

        match visibleTreesInFrontIndex with
        | None -> treesInFront.Length
        | Some index -> index + 1

let getScenicScore ((xIndex, yIndex): int * int) : int =
    let tree = treeGrid[yIndex][xIndex]

    let frontX, backX, frontY, backY =
        getSurroundingTrees (xIndex, yIndex)

    let visibleTreesToLeft =
        getVisibleTreesInFront (frontX |> Array.rev) tree

    let visibleTreesToRight =
        getVisibleTreesInFront backX tree

    let visibleTreesToTop =
        getVisibleTreesInFront (frontY |> Array.rev) tree

    let visibleTreesToBottom =
        getVisibleTreesInFront backY tree

    visibleTreesToLeft
    * visibleTreesToRight
    * visibleTreesToTop
    * visibleTreesToBottom

let part1Result =
    treeGrid
    |> Array.mapi (fun yIndex xTrees ->
        xTrees
        |> Array.mapi (fun xIndex _ -> isVisible (xIndex, yIndex)))
    |> Array.concat
    |> Array.filter id
    |> Array.length

let part2Result =
    treeGrid
    |> Array.mapi (fun yIndex xTrees ->
        xTrees
        |> Array.mapi (fun xIndex _ -> getScenicScore (xIndex, yIndex)))
    |> Array.concat
    |> Array.max

printfn $"Part 1 result: {part1Result}"
printfn $"Part 2 result: {part2Result}"
