module Cities

let loadCities file =  
    System.IO.File.ReadLines file
    |> Seq.map (fun (line:System.String) -> line.Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
    |> Seq.map Array.toList
    |> Seq.toList
