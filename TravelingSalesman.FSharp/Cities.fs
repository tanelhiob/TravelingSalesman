module Cities

let loadCities file amount =  
    System.IO.File.ReadLines file
    |> Seq.map (fun (line:System.String) -> line.Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.take amount |> Array.map int)
    |> Seq.map Array.toList
    |> Seq.take amount
    |> Seq.toList
