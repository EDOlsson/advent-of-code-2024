let testInput = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

type CalibrationEquation = 
    { Answer : uint64
      Operands : uint64 list }

let parseInput (input : string) =
    input.Split('\n')
    |> Seq.map (fun l -> l.Split(':'))
    |> Seq.map (fun a -> { Answer = uint64 a.[0]; Operands = (a.[1]).Split(' ') |> Array.skip(1) |> Array.map uint64 |> Seq.toList })

let concat (o1 : uint64) (o2 : uint64) =
    uint64 $"{o1}{o2}"

let solveSingleEquation (equation : CalibrationEquation) =
    let rec solveSingleEquation' (operands : uint64 list) (answer : uint64) =
        match operands with
        | [] -> answer
        | [h] -> if answer + h = equation.Answer
                    then
                        equation.Answer
                    else if answer * h = equation.Answer
                    then
                        equation.Answer
                    else if
                        concat answer h = equation.Answer
                    then
                        equation.Answer
                    else
                        uint64 0
        | h::t -> if solveSingleEquation' t (answer + h) = equation.Answer
                    then
                        equation.Answer
                    else if solveSingleEquation' t (answer * h) = equation.Answer
                    then
                        equation.Answer
                    else if solveSingleEquation' t (concat answer h) = equation.Answer
                    then
                        equation.Answer
                    else
                        uint64 0

    let answer = solveSingleEquation' (equation.Operands |> List.skip 1) (equation.Operands |> List.head)
    (answer = equation.Answer, answer)

let testResult = testInput
                |> parseInput
                |> Seq.map solveSingleEquation
                |> Seq.filter (fun (b, _) -> b)
                |> Seq.sumBy (fun (_, a) -> a)

printfn "%d" testResult

let part2 = System.IO.File.ReadAllText "day7-input"
            |> parseInput
            |> Seq.map solveSingleEquation
            |> Seq.filter (fun (b, _) -> b)
            |> Seq.sumBy (fun (_, a) -> a)

printfn "%d" part2
