let testInput = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

type Coordinate = { X : int; Y : int }
type Space = Empty | Obstruction | GuardUp | GuardDown | GuardLeft | GuardRight
type MapLocation = { Space : Space; IsTraveled : bool }

let parseInput (input : string) =
    let parseSpace (c : char) =
        match c with
        | '.' -> Empty
        | '#' -> Obstruction
        | '^' -> GuardUp
        | 'v' -> GuardDown
        | '<' -> GuardLeft
        | '>' -> GuardRight
        | _ -> failwith "Invalid space"

    let mapLocations = input.Split('\n')
                        |> Seq.mapi (fun y l -> l.ToCharArray() |> Seq.mapi (fun x c -> ( { X = x; Y = y }, { Space = parseSpace c; IsTraveled = c <> '.' && c <> '#' })))
                        |> Seq.collect id

    Map mapLocations

let renderMap (map : Map<Coordinate, MapLocation>) =
    let xLimit = map |> Map.keys |> Seq.maxBy (fun k -> k.X) |> fun k -> k.X
    let yLimit = map |> Map.keys |> Seq.maxBy (fun k -> k.Y) |> fun k -> k.Y
    let rows = seq { for y in 0 .. yLimit do
                        yield seq { for x in 0 .. xLimit do
                                    let location = { X = x; Y = y }
                                    match map |> Map.tryFind location with
                                    | None -> ' '
                                    | Some l -> match l.Space with
                                                | Empty -> (if l.IsTraveled then 'X' else '.')
                                                | Obstruction -> '#'
                                                | GuardUp -> '^'
                                                | GuardDown -> 'v'
                                                | GuardLeft -> '<'
                                                | GuardRight -> '>' }
                        |> Seq.toArray |> System.String }

    System.String.Join("\n", rows)

let countGuardStepsInOneDirection (map : Map<Coordinate, MapLocation>) =
    let advanceGuardOneStep (map : Map<Coordinate, MapLocation>) =
        let guardCoordinate = map |> Map.findKey (fun _ v -> v.Space = GuardUp || v.Space = GuardDown || v.Space = GuardLeft || v.Space = GuardRight)
        let guard = map[guardCoordinate]

        let renderGuardLocation (guardLocation : MapLocation) (nextLocation : MapLocation) =
            let rotateGuard (guard : Space) =
                match guard with
                | GuardUp -> GuardRight
                | GuardRight -> GuardDown
                | GuardDown -> GuardLeft
                | GuardLeft -> GuardUp
                | _ -> failwith "Invalid guard"

            if nextLocation.Space = Obstruction then
                { guardLocation with Space = (rotateGuard guardLocation.Space); IsTraveled = true }
            else
                { guardLocation with Space = Empty; IsTraveled = true }

        let renderNextLocation (guardLocation : MapLocation) (nextLocation : MapLocation) =
            if nextLocation.Space = Empty then
                { nextLocation with Space = guardLocation.Space; IsTraveled = true }
            else
                nextLocation

        let nextGuardCoordinate = match guard.Space with
                                  | GuardUp -> { X = guardCoordinate.X; Y = guardCoordinate.Y - 1 }
                                  | GuardDown -> { X = guardCoordinate.X; Y = guardCoordinate.Y + 1 }
                                  | GuardLeft -> { X = guardCoordinate.X - 1; Y = guardCoordinate.Y }
                                  | GuardRight -> { X = guardCoordinate.X + 1; Y = guardCoordinate.Y }
                                  | _ -> failwith "Invalid guard"
        
        let nextLocation = map |> Map.tryFind nextGuardCoordinate

        // printfn "%s" <| renderMap map
        // printfn ""

        match nextLocation with
        | None -> (map, nextGuardCoordinate)
        | Some n ->
                    let nextGuardLocation = renderNextLocation guard n
                    let oldGuardLocation = renderGuardLocation guard n
                    (map |> Map.add guardCoordinate oldGuardLocation |> Map.add nextGuardCoordinate nextGuardLocation, nextGuardCoordinate)

    let xLimit = map |> Map.keys |> Seq.maxBy (fun k -> k.X) |> fun k -> k.X
    let yLimit = map |> Map.keys |> Seq.maxBy (fun k -> k.Y) |> fun k -> k.Y
    let iterationLimit = 10_000

    let rec advanceGuard (map : Map<Coordinate, MapLocation>) (iterationCount : int) =
        if iterationCount >= iterationLimit then
            printfn "Iteration limit reached"
            map
        else
            let (newMap, nextSpace) = advanceGuardOneStep map

            match nextSpace with
            | n when n.X < 0 || n.X > xLimit || n.Y < 0 || n.Y > yLimit -> newMap
            | _ -> advanceGuard newMap (iterationCount + 1)

    advanceGuard map 0

let countSpacesTraveled (map : Map<Coordinate, MapLocation>) =
    map
    |> Map.values
    |> Seq.filter (fun l -> l.IsTraveled)
    |> Seq.length

(*
let finalMap = parseInput testInput
              |> countGuardStepsInOneDirection

let testResult = countSpacesTraveled finalMap

printfn "%d" testResult

printfn "Final map"
printfn "------------------------------"
printfn ""
printfn "%s" <| renderMap finalMap
*)

#time
let part1 = System.IO.File.ReadAllText "day6-input"
            |> parseInput
            |> countGuardStepsInOneDirection
            |> countSpacesTraveled
#time

printfn "%d" part1
