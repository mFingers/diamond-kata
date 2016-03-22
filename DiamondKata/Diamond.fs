module Diamond

open System

let make letter =
    let letters = ['A' .. letter]

    letters @ (letters |> List.rev |> List.tail)
    |> List.map string
    |> List.reduce (fun x y -> sprintf "%s%s%s" x Environment.NewLine y)