let testInput = """3   4
4   3
2   5
1   3
3   9
3   3"""

let findFrequencies n otherList =
    otherList
    |> Array.filter(fun x -> x = n)
    |> Array.length

let input = System.IO.File.ReadAllText "day1-input"

let list1, list2 = input.Split('\n')
                    |> Array.map(fun x -> x.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))
                    |> Array.map(fun x -> (int x.[0], int x.[1]))
                    |> Array.unzip

let part2 = list1
            |> Array.map(fun x -> x * (findFrequencies x list2))
            |> Array.sum

printfn "Part 2 : %d" part2
part2
