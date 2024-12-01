let testInput = """3   4
4   3
2   5
1   3
3   9
3   3"""

let sumOfDistances l1 l2 = l1
                            |> Array.sort
                            |> Array.zip (l2 |> Array.sort)
                            |> Array.sumBy(fun (x, y) -> abs(x - y))

let input = System.IO.File.ReadAllText "day1-input"

let list1, list2 = input.Split('\n')
                    |> Array.map(fun x -> x.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))
                    |> Array.map(fun x -> (int x.[0], int x.[1]))
                    |> Array.unzip

let result = sumOfDistances list1 list2
printfn "Result %d" result
result
