let testInput = "125 17"

let parseInput (input : string) =
    input.Split(' ')
    |> Array.map uint64
    |> Array.toList

let handleBlink (stones : uint64 list) =
    let handleStone (stone : uint64) =
        if stone = (uint64 0) then
            [(uint64 1)]
        else if (String.length ($"{stone}")) % 2 = 0 then
            let stoneAsString = string stone
            let half = (String.length stoneAsString) / 2
            let firstHalf = stoneAsString.Substring(0, half)
            let secondHalf = stoneAsString.Substring(half)
            let firstHalfAsInt = uint64 firstHalf
            let secondHalfAsInt = uint64 secondHalf
            [ firstHalfAsInt; secondHalfAsInt ]
        else
            [stone * (uint64 2024)]

    [ 1..25 ]
    |> List.fold (fun acc _ -> acc |> List.collect handleStone) stones

(*
let stones = parseInput testInput

let testResult = handleBlink stones
                 |> List.length
*)

let stones = parseInput <| System.IO.File.ReadAllText "dayB-input"

let part1 = handleBlink stones
            |> List.length

printfn "%A" part1
