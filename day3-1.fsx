let testInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let parseInstructions (input : string) =
    let r = System.Text.RegularExpressions.Regex(@"mul\((\d+),(\d+)\)")

    r.Matches input
    |> Seq.cast<System.Text.RegularExpressions.Match>
    |> Seq.map fun m -> (int m.Groups.[1].Value, int m.Groups.[2].Value)
    |> Seq.map fun (a, b) -> a * b

let testResult = testInput
                |> parseInstructions
                |> Seq.sum

printfn "Test : %d" testResult

let input = System.IO.File.ReadAllText "day3-input"
let part1 = input
            |> parseInstructions
            |> Seq.sum

printfn "Part 1 : %d" part1
