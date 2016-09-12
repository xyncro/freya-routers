[<AutoOpen>]
module Freya.Routers.Uri.Template.Tests.Prelude

open Aether
open Freya.Core

// Optics

let defaultValue : Lens<State,string option> =
    State.value_ "default"

// Fixtures

let defaultSetup =
    Freya.empty