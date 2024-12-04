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

let countVerticalXmasDown (input : char array array) =
    let mutable count = 0
    for i in 0..input.Length - 1 do
        for j in 0..input.[i].Length - 4 do
            if input.[i].[j] = 'X' && input.[i].[j + 1] = 'M' && input.[i].[j + 2] = 'A' && input.[i].[j + 3] = 'S' then
                count <- count + 1
    count

let countVerticalXmasUp (input : char array array) =
    let mutable count = 0
    for i in 0..input.Length - 1 do
        for j in 0..input.[i].Length - 4 do
            if input.[i].[j] = 'S' && input.[i].[j + 1] = 'A' && input.[i].[j + 2] = 'M' && input.[i].[j + 3] = 'X' then
                count <- count + 1
    count

let countHorizontalXmasRight (input : char array array) =
    let mutable count = 0
    for i in 0..input.Length - 4 do
        for j in 0..input.[i].Length - 1 do
            if input.[i].[j] = 'X' && input.[i + 1].[j] = 'M' && input.[i + 2].[j] = 'A' && input.[i + 3].[j] = 'S' then
                count <- count + 1
    count

let countHorizontalXmasLeft (input : char array array) =
    let mutable count = 0
    for i in 0..input.Length - 4 do
        for j in 0..input.[i].Length - 1 do
            if input.[i].[j] = 'S' && input.[i + 1].[j] = 'A' && input.[i + 2].[j] = 'M' && input.[i + 3].[j] = 'X' then
                count <- count + 1
    count

let countDiagonalXmasDownRight (input : char array array) =
    let mutable count = 0
    for i in 0..input.Length - 4 do
        for j in 0..input.[i].Length - 4 do
            if input.[i].[j] = 'X' && input.[i + 1].[j + 1] = 'M' && input.[i + 2].[j + 2] = 'A' && input.[i + 3].[j + 3] = 'S' then
                count <- count + 1
    count

let countDiagonalXmasDownLeft (input : char array array) =
    let mutable count = 0
    for i in 0..input.Length - 4 do
        for j in 3..input.[i].Length - 1 do
            if input.[i].[j] = 'X' && input.[i + 1].[j - 1] = 'M' && input.[i + 2].[j - 2] = 'A' && input.[i + 3].[j - 3] = 'S' then
                count <- count + 1
    count

let countDiagonalXmasUpRight (input : char array array) =
    let mutable count = 0
    for i in 3..input.Length - 1 do
        for j in 0..input.[i].Length - 4 do
            if input.[i].[j] = 'X' && input.[i - 1].[j + 1] = 'M' && input.[i - 2].[j + 2] = 'A' && input.[i - 3].[j + 3] = 'S' then
                count <- count + 1
    count

let countDiagonalXmasUpLeft (input : char array array) =
    let mutable count = 0
    for i in 3..input.Length - 1 do
        for j in 3..input.[i].Length - 1 do
            if input.[i].[j] = 'X' && input.[i - 1].[j - 1] = 'M' && input.[i - 2].[j - 2] = 'A' && input.[i - 3].[j - 3] = 'S' then
                count <- count + 1
    count

let countXmas (input : char array array) =
    countVerticalXmasDown input +
    countVerticalXmasUp input +
    countHorizontalXmasRight input +
    countHorizontalXmasLeft input +
    countDiagonalXmasDownRight input +
    countDiagonalXmasDownLeft input +
    countDiagonalXmasUpRight input +
    countDiagonalXmasUpLeft input

let testResult = testInput
                |> parseInput
                |> countXmas

printfn "Test : %d" testResult

let input = System.IO.File.ReadAllText "day4-input"
let part1 = input
            |> parseInput
            |> countXmas

printfn "Part 1 : %d" part1
