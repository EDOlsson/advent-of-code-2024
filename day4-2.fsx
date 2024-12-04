let testInput = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

let parseInput (input : string) =
    input.Split('\n')
    |> Seq.map (fun s -> s.ToCharArray())
    |> Seq.toArray

let findXmas (input : char array array) =
    let mutable xmasCount = 0

    for i in 0..input.Length - 3 do
        for j in 0..input.[i].Length - 3 do
            if input.[i].[j] = 'M' && input.[i+2].[j] = 'M' && input.[i+1].[j+1] = 'A' && input.[i].[j+2] = 'S' && input.[i+2].[j+2] = 'S' then
                xmasCount <- xmasCount + 1

    xmasCount

let findXmasForwardsBackwards (input : char array array) =
    let mutable count = 0

    for i in 0..input.Length - 3 do
        for j in 0..input.[i].Length - 3 do
            if input.[i].[j] = 'M' && input.[i+2].[j] = 'S' && input.[i+1].[j+1] = 'A' && input.[i].[j+2] = 'M' && input.[i+2].[j+2] = 'S' then
                count <- count + 1

    count

let findXmasBackwardsBackwards (input : char array array) =
    let mutable count = 0

    for i in 0..input.Length - 3 do
        for j in 0..input.[i].Length - 3 do
            if input.[i].[j] = 'S' && input.[i+2].[j] = 'S' && input.[i+1].[j+1] = 'A' && input.[i].[j+2] = 'M' && input.[i+2].[j+2] = 'M' then
                count <- count + 1

    count

let findXmasBackwardsForwards (input : char array array) =
    let mutable count = 0

    for i in 0..input.Length - 3 do
        for j in 0..input.[i].Length - 3 do
            if input.[i].[j] = 'S' && input.[i+2].[j] = 'M' && input.[i+1].[j+1] = 'A' && input.[i].[j+2] = 'S' && input.[i+2].[j+2] = 'M' then
                count <- count + 1

    count

let countXmas (input : char array array) =
    findXmas input +
    findXmasForwardsBackwards input +
    findXmasBackwardsBackwards input +
    findXmasBackwardsForwards input

let testResult = testInput
                |> parseInput
                |> countXmas

printfn "Test : %d" testResult

let input = System.IO.File.ReadAllText "day4-input"
let part2 = input
            |> parseInput
            |> countXmas

printfn "Part 2 : %d" part2
