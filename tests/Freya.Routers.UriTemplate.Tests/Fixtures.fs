[<AutoOpen>]
module Freya.Routers.UriTemplate.Tests.Fixtures

open Arachne.Uri

let route1 =
    set (Some 1)

let route2 =
    set (Some 2)

let route3 =
    set (Some 3)

let emptyQuery =
    Query ""