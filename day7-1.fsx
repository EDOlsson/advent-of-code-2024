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
    { Answer : int64
      Operands : int64 list }

let parseInput (input : string) =
    input.Split('\n')
    |> Seq.map (fun l -> l.Split(':'))
    |> Seq.map (fun a -> { Answer = int64 a.[0]; Operands = (a.[1]).Split(' ') |> Array.skip(1) |> Array.map int64 |> Seq.toList })

let solveSingleEquation (equation : CalibrationEquation) =
    let rec solveSingleEquation' (operands : int64 list) (operator : int64 -> int64 -> int64) (otherOperator : int64 -> int64 -> int64) (answer : int64) =
        match operands with
        | [] -> answer
        | [h] -> if operator answer h = equation.Answer then equation.Answer else if otherOperator answer h = equation.Answer then equation.Answer else answer
        | h::t -> if solveSingleEquation' t operator otherOperator (operator answer h) = equation.Answer then equation.Answer else if solveSingleEquation' t operator otherOperator (otherOperator answer h) = equation.Answer then equation.Answer else answer

    let addOperator = (+)
    let multiplyOperator = (*)

    let answer = solveSingleEquation' (equation.Operands |> List.skip 1) addOperator multiplyOperator (equation.Operands |> List.head)
    (answer = equation.Answer, answer)

(*
let testResult = testInput
                |> parseInput
                |> Seq.map solveSingleEquation
                |> Seq.filter (fun (b, _) -> b)
                |> Seq.map (fun (_, a) -> a)
                |> Seq.sum

printfn "%d" testResult
*)

let part1 = System.IO.File.ReadAllText "day7-input"
            |> parseInput
            |> Seq.map solveSingleEquation
            |> Seq.filter (fun (b, _) -> b)
            |> Seq.map (fun (_, a) -> a)
            |> Seq.sum

printfn "%d" part1
