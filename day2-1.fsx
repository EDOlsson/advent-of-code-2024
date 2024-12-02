let testInput = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""

let parseInput (input : string) =
    input.Split('\n')
    |> Array.map(fun x -> x.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))
    |> Array.map(fun x -> x |> Array.map int)

let isReportSafe report =
    let differences = report
                        |> Array.pairwise 
                        |> Array.map (fun (a, b) -> a - b)

    let isMonotonicIncreasing = differences |> Array.forall (fun x -> x > 0)
    let isMonotonicDecreasing = differences |> Array.forall (fun x -> x < 0)

    let isMonotic = isMonotonicIncreasing || isMonotonicDecreasing
    let isSmall = differences
                    |> Array.map abs
                    |> Array.forall (fun x -> x >= 1 && x <= 3)

    // printfn "%A : is monotonic? %b; isSmall %b" report isMonotic isSmall

    isMonotic && isSmall

let testResult = testInput
                |> parseInput
                |> Array.map isReportSafe
                |> Array.filter (fun x -> x)
                |> Array.length

printfn "Test : %d" testResult

let input = System.IO.File.ReadAllText "day2-input"
let part1 = input
            |> parseInput
            |> Array.map isReportSafe
            |> Array.filter (fun x -> x)
            |> Array.length

printfn "Part 1 : %d" part1
