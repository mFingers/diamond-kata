module DiamondProperties

open System
open FsCheck
open FsCheck.Xunit

let stringExists = not << String.IsNullOrWhiteSpace

let split (x:string) =
    x.Split([| Environment.NewLine |], StringSplitOptions.None)

let trim (x:string) = x.Trim()

let leadingSpaces (x:string) =
    let indexOfNonSpace = x.IndexOfAny([| 'A' .. 'Z' |])
    x.Substring(0, indexOfNonSpace)

let trailingSpaces (x:string) =
    let indexOfNonSpace = x.LastIndexOfAny([| 'A' .. 'Z' |])
    x.Substring(indexOfNonSpace+1)

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

[<UpperCaseCharProperty>]
let ``All rows must be vertically symmetrical`` (letter:char) =
    let actual = Diamond.make letter

    let rows = split actual
    rows |> Array.forall (fun r -> (leadingSpaces r) = (trailingSpaces r))

[<UpperCaseCharProperty>]
let ``Top of figure has correct letters in correct order`` (letter:char) =
    let actual = Diamond.make letter

    let expected = ['A' .. letter]

    let rows = split actual
    let firstNonWhiteSpaceLetters =
        rows
        |> Seq.take expected.Length
        |> Seq.map trim
        |> Seq.map Seq.head
        |> Seq.toList 

    expected = firstNonWhiteSpaceLetters

[<UpperCaseCharProperty>]
let ``All rows must be horizontally symmetrical`` (letter:char) =
    let actual = Diamond.make letter

    let rows = split actual
    let topRows =
        rows
        |> Seq.takeWhile (fun r -> not (r.Contains(string letter)))
        |> Seq.toList

    let bottomRows =
        rows
        |> Seq.skipWhile (fun r -> not (r.Contains(string letter)))
        |> Seq.skip 1
        |> Seq.toList
        |> Seq.rev
        |> Seq.toList

    topRows = bottomRows

[<UpperCaseCharProperty>]
let ``Diamond is as wide as it is high`` (letter:char) =
    let actual = Diamond.make letter

    let rows = split actual
    let expected = rows.Length
    rows |> Array.forall (fun r -> r.Length = expected)

[<UpperCaseCharProperty>]
let ``All rows except top and bottom have two identical letters`` (letter:char) =
    let actual = Diamond.make letter

    let isTwoIdenticalLetters x =
        let hasIdenticalLetters = x |> Seq.distinct |> Seq.length = 1
        let hasTwoLetters = x |> Seq.length = 2
        hasIdenticalLetters && hasTwoLetters

    let rows = split actual
    rows
    |> Array.filter (fun x -> not (x.Contains("A")))
    |> Array.map (fun x -> x.Replace(" ", ""))
    |> Array.forall isTwoIdenticalLetters

[<UpperCaseCharProperty>]
let ``Lower left space is a triangle`` (letter:char) =
    let actual = Diamond.make letter

    let rows = split actual
    let lowerLeftSpace =
        rows
        |> Seq.skipWhile (fun x -> not (x.Contains(string letter)))
        |> Seq.map leadingSpaces
        |> Seq.toList

    let spaceCounts = lowerLeftSpace |> List.map (fun x -> x.Length)
    let expected =
        Seq.initInfinite id
        |> Seq.take spaceCounts.Length
        |> Seq.toList

    expected = spaceCounts