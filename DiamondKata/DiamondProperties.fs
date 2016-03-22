module DiamondProperties

open System
open FsCheck
open FsCheck.Xunit

let stringExists = not << String.IsNullOrWhiteSpace

let split (x:string) =
    x.Split([| Environment.NewLine |], StringSplitOptions.None)

let trim (x:string) = x.Trim()

type Letters =
    static member Char() =
        Arb.Default.Char()
        |> Arb.filter (fun c -> 'A' <= c && c <= 'Z')

type UpperCaseCharPropertyAttribute() =
    inherit PropertyAttribute(Arbitrary = [| typeof<Letters> |] )

[<UpperCaseCharProperty>]
let ``Diamond is non-empty`` (letter:char) =
    let actual = Diamond.make letter
    stringExists actual

[<UpperCaseCharProperty>]
let ``First row contains 'A'`` (letter:char) =
    let actual = Diamond.make letter

    let rows = split actual
    rows |> Seq.head |> trim = "A"