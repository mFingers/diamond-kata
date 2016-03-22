module Diamond

open System

let make letter =
    ['A' .. letter]
    |> List.map string
    |> List.reduce (fun x y -> sprintf "%s%s%s" x Environment.NewLine y)