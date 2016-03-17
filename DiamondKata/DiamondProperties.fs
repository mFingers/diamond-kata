module DiamondProperties

open System
open FsCheck.Xunit

let stringExists = not << String.IsNullOrWhiteSpace

[<Property>]
let ``Diamond is non-empty`` (letter:char) =
    let actual = Diamond.make letter
    stringExists actual