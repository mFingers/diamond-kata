module Diamond

open System

let make letter =
    let makeLine letterCount (letter, letterIndex) =
        let leadingSpace = String(' ', letterCount - 1 - letterIndex)
        let innerSpace = String(' ', letterCount - 1 - leadingSpace.Length)
        match letter with
        | 'A' ->
            sprintf "%s%c%s" leadingSpace letter leadingSpace
        | _   -> 
            let left = sprintf "%s%c%s" leadingSpace letter innerSpace |> Seq.toList
            left @ (left |> List.rev |> List.tail)
            |> List.map string
            |> List.reduce (sprintf "%s%s")
             
    let letters = ['A' .. letter] |> List.mapi (fun i l -> l, i)

    letters @ (letters |> List.rev |> List.tail)
    |> List.map (makeLine letters.Length)
    |> List.reduce (fun x y -> sprintf "%s%s%s" x Environment.NewLine y)