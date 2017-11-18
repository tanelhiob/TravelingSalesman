open Cities

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

    let rec findPathRec path =

        if List.length path = cities.Length then
            let possiblePath = path @ [ 0 ]
            let distance = calculateTotalDistance cities possiblePath

            if distance < bestLength then
                bestLength <- distance
                Some possiblePath
            else
                None

        else
            let unvisitedCities = [0 .. cities.Length - 1] |> List.except path
            let possiblePaths =
                [ for unvisitedCity in unvisitedCities ->
                    let possiblePath = path @ [ unvisitedCity ]
                    let distance = calculateTotalDistance cities possiblePath
                    if distance < bestLength then
                        findPathRec possiblePath
                    else
                        None
                ]

            let paths = possiblePaths |> List.filter Option.isSome |> List.map Option.get
            if paths.IsEmpty then
                None
            else
                paths |> List.minBy (calculateTotalDistance cities) |> Some

    Option.get (findPathRec [ 0 ])

[<EntryPoint>]
let main _ = 

    let cities = loadCities "Data\\eesti.in"

    let greedyPath = findGreedyPath cities
    let greedyTotalDistance = calculateTotalDistance cities greedyPath
    printfn "%A %i" greedyPath greedyTotalDistance

    //let bestPath = findBestPath cities
    //let bestPathTotalDistance = calculateTotalDistance cities bestPath
    //printfn "%A %i" bestPath bestPathTotalDistance

    //let bestOptimalPath = findBestOptimalPath cities
    //let bestOptimalPathTotalDistance = calculateTotalDistance cities bestPath
    //printfn "%A %i" bestOptimalPath bestOptimalPathTotalDistance

    0 // exit gracefully