[<AutoOpen>]
module Freya.Routers.UriTemplate.Tests.Prelude

open Aether
open Freya.Core
open Freya.Core.Operators

(* Optics *)

let defaultValue : Lens<State,string option> =
    State.value_ "default"

(* Fixtures *)

let defaultSetup =
    Freya.empty