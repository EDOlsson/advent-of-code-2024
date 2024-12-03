let testInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let isInstructionEnabled (index : int) (doInstructions : int seq) (dontInstructions : int seq) =
    let precedingDoInstructions = doInstructions
                                  |> Seq.filter (fun i -> i < index)

    let precedingDontInstructions = dontInstructions
                                    |> Seq.filter (fun i -> i < index)

    if (Seq.isEmpty precedingDontInstructions)
    then
        true
    else
        let nearestDoInstructionIndex = if (Seq.isEmpty precedingDoInstructions) then 0 else Seq.max precedingDoInstructions
        let nearestDontInstructionIndex = Seq.max precedingDontInstructions

        let doDelta = index - nearestDoInstructionIndex
        let dontDelta = index - nearestDontInstructionIndex

        doDelta < dontDelta

let parseInstructions (input : string) =
    let mulInstructions = System.Text.RegularExpressions.Regex(@"mul\((\d+),(\d+)\)")
    let mulResults = mulInstructions.Matches input
                     |> Seq.cast<System.Text.RegularExpressions.Match>
                     |> Seq.map (fun m -> (m.Groups.[0].Index, int m.Groups.[1].Value, int m.Groups.[2].Value))
                     |> Seq.map (fun (i, a, b) -> (i, a * b))

    let doInstructions = System.Text.RegularExpressions.Regex @"do\(\)"
    let doResults = doInstructions.Matches input
                    |> Seq.cast<System.Text.RegularExpressions.Match>
                    |> Seq.map (fun m -> int m.Groups.[0].Index)

    let dontInstructions = System.Text.RegularExpressions.Regex @"don't\(\)"
    let dontResults = dontInstructions.Matches input
                      |> Seq.cast<System.Text.RegularExpressions.Match>
                      |> Seq.map (fun m -> int m.Groups.[0].Index)

    mulResults
    |> Seq.filter (fun (i, _) -> isInstructionEnabled i doResults dontResults)
    |> Seq.map snd

let testResult = testInput
                |> parseInstructions
                |> Seq.sum

printfn "Test : %d" testResult

let input = System.IO.File.ReadAllText "day3-input"
let part2 = input
            |> parseInstructions
            |> Seq.sum

printfn "Part 2 : %d" part2
