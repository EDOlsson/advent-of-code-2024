let testInput = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

type PageOrderingRule =
    { Page1 : int
      Page2 : int }

type UpdatePages =
    { PageNumbers : int list }

type Day5Input =
    { PageOrderingRules : PageOrderingRule seq
      UpdatePages : UpdatePages seq }

let parseInput (input : string) =
    let parsePageOrderingRules (input : string) =
        input.Split('\n')
        |> Seq.takeWhile (fun l -> l.Length > 0)
        |> Seq.map (fun l -> l.Split('|'))
        |> Seq.map (fun a -> { Page1 = int a.[0]; Page2 = int a.[1] })

    let parseUpdatePages (input : string) =
        input.Split('\n')
        |> Seq.skipWhile (fun l -> l.Length > 0)
        |> Seq.skip 1
        |> Seq.map (fun l -> l.Split(',', System.StringSplitOptions.RemoveEmptyEntries))
        |> Seq.map (fun a -> { PageNumbers = a |> Seq.map int |> Seq.toList })

    { PageOrderingRules = parsePageOrderingRules input
      UpdatePages = parseUpdatePages input }

let areUpdatePagesValid (updatePages : UpdatePages) (pageOrderingRules : PageOrderingRule seq) =
    let indexedPages = updatePages.PageNumbers |> List.indexed

    pageOrderingRules
    |> Seq.forall (fun r -> 
        let page1Index = indexedPages |> List.tryFindIndex (fun (_, p) -> p = r.Page1)
        let page2Index = indexedPages |> List.tryFindIndex (fun (_, p) -> p = r.Page2)

        match page1Index, page2Index with
        | Some i1, Some i2 -> i1 < i2
        | _ -> true)    // ignore this rule if the pages are not in the update list

let findMiddlePage (updatePages : UpdatePages) =
    let pageCount = List.length updatePages.PageNumbers

    updatePages.PageNumbers
    |> List.skip (pageCount / 2)
    |> List.head

let day5TestInput = testInput
                    |> parseInput

let testResult = day5TestInput.UpdatePages
                    |> Seq.filter (fun up -> areUpdatePagesValid up day5TestInput.PageOrderingRules)
                    |> Seq.map findMiddlePage
                    |> Seq.sum

printfn "Test result: %d" testResult

let day5Input = System.IO.File.ReadAllText "day5-input"
                |> parseInput

let part1 = day5Input.UpdatePages
            |> Seq.filter (fun up -> areUpdatePagesValid up day5Input.PageOrderingRules)
            |> Seq.map findMiddlePage
            |> Seq.sum

printfn "Part 1: %d" part1
