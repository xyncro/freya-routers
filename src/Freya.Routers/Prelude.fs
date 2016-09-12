namespace Freya.Routers

open Aether

// Prelude

// Functions

[<AutoOpen>]
module Functions =

    let apply f a =
        f a

    let pair a b =
        a, b

    let uncurry f (a, b) =
        f a b

// List

[<RequireQualifiedAccess>]
module List =

    let pair (a, b) =
        a :: [ b ]

    let extract e =
        List.partition (Optic.get (Prism.ofEpimorphism e) >> Option.isSome)
     >> function | [], xs -> [], xs
                 | ys, xs -> List.map (Optic.get (Prism.ofEpimorphism e) >> Option.get) ys, xs

// Option

[<RequireQualifiedAccess>]
module Option =

    let unsafe_ : Isomorphism<'a option,'a> =
        Option.get, Some