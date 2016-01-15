module Freya.Routers

open Aether

(* Prelude *)

[<AutoOpen>]
module Prelude =

    (* Option *)

    [<RequireQualifiedAccess>]
    module Option =

        let unsafe_ : Isomorphism<'a option,'a> =
            Option.get, Some
