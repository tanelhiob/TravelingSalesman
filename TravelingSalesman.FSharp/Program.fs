open Cities
open System.Diagnostics

let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

let rec pair = function
    | fst::snd::xs -> (fst, snd) :: pair (snd::xs)
    | _ -> []

let calculateTotalDistance (cities:int list list) path =
    path
    |> pair
    |> List.map (fun (previous, next) -> cities.[previous].[next])
    |> List.reduce (+)

let findGreedyPath (cities:int list list) =

    let rec findGreedyPathRecursive (unvisitedCities: int list) currentCity =

        printfn "greedy depth %i" (cities.Length - unvisitedCities.Length) 

        if unvisitedCities.Length = 1 then
            unvisitedCities
        else
            let closestUnvisitedCity = List.minBy (fun city -> cities.[currentCity].[city]) unvisitedCities
            let remainingUnvisitedCities = unvisitedCities |> List.except [closestUnvisitedCity]
            closestUnvisitedCity :: (findGreedyPathRecursive remainingUnvisitedCities closestUnvisitedCity)
             
    let startingCity = 0
    let path = findGreedyPathRecursive [0 .. cities.Length - 1] startingCity
    path @ [0]

let findBestPath (cities:int list list) =

    let permutated = permute [1 .. cities.Length - 1]
    let allPaths = permutated |> List.map (fun p -> [0] @ p @ [0])
    let bestPath = allPaths |> List.minBy (calculateTotalDistance cities)

    bestPath

let findBestOptimalPath (cities:int list list) =

    let mutable bestLength = System.Int32.MaxValue

    let rec findPathRec path pathLength =

        if List.length path = cities.Length then

            let possiblePath = path @ [ List.head path ]            
            let distance = pathLength + cities.[(List.last path)].[List.head path]

            if distance < bestLength then
                bestLength <- distance
                printfn "%s %i %A" (System.DateTime.Now.ToString("HH:mm:ss.ffff")) bestLength possiblePath
                Some (possiblePath, distance)
            else
                None
        else
            let preferredUnvisitedCities =
                [0 .. cities.Length - 1]
                    |> List.except path
                    |> List.map (fun city -> (city, cities.[(List.last path)].[city]))
                    |> List.filter (fun (_, distance) -> distance < bestLength)
                    |> List.sortBy (fun (_, distance) -> distance)

            let possiblePaths =
                preferredUnvisitedCities
                    |> List.map (fun (city, distance) -> findPathRec (path @ [ city ]) (distance + pathLength))


            let paths = possiblePaths |> List.filter Option.isSome |> List.map Option.get
            if List.isEmpty paths then
                None
            else
                paths |> List.minBy (fun (_, distance) -> distance) |> Some
    
    match cities.Length with
    | len when len < 2 -> [0]
    | _ -> fst (Option.get (findPathRec [0] 0))

let takeTime operation =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    operation
    stopWatch.Stop()

    printfn "operation took %ims" stopWatch.ElapsedMilliseconds


[<EntryPoint>]
let main _ = 

    let cities = loadCities "Data\\eesti.in" 20
    // printfn "%i" (calculateTotalDistance cities [0; 14; 2; 3; 1; 11; 13; 4; 9; 18; 17; 7; 6; 8; 19; 15; 16; 5; 10; 12; 0])
    // 1455
    

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    //let greedyPath = findGreedyPath cities
    //let greedyTotalDistance = calculateTotalDistance cities greedyPath
    //printfn "%A %i" greedyPath greedyTotalDistance


    //let bestPath = findBestPath cities
    //let bestPathTotalDistance = calculateTotalDistance cities bestPath
    //printfn "%A %i" bestPath bestPathTotalDistance

    let bestOptimalPath = findBestOptimalPath cities
    let bestOptimalPathTotalDistance = calculateTotalDistance cities bestOptimalPath
    printfn "%A %i" bestOptimalPath bestOptimalPathTotalDistance

    stopWatch.Stop()
    printfn "operation took %ims" stopWatch.ElapsedMilliseconds

    0 // exit gracefully