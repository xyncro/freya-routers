[<AutoOpen>]
module Freya.Routers.UriTemplate.Tests.Routing

open Arachne.Http
open Freya.Core
open Freya.Routers.UriTemplate
open Swensen.Unquote
open Xunit

(* Tests *)

[<Fact>]
let ``Router With No Routes Returns Next`` () =
    let routes =
        freyaRouter {
            return () }

    result GET "/" emptyQuery routes =! Next

[<Fact>]
let ``Router With Base Route Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" route1 }

    let v = value GET "/" emptyQuery routes

    v =! Some 1

[<Fact>]
let ``Router With Multiple Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" route1
            route All "/some/path" route2
            route All "/other/path" route3 }

    value GET "/" emptyQuery routes =! Some 1
    value GET "/some/path" emptyQuery routes =! Some 2
    value GET "/other/path" emptyQuery routes =! Some 3
    value GET "/unset/path" emptyQuery routes =! None

[<Fact>]
let ``Router With Method Specific Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route GET "/" route1
            route GET "/some/path" route2
            route POST "/some/path" route3 }

    value GET "/" emptyQuery routes =! Some 1
    value POST "/" emptyQuery routes =! None
    value GET "/some/path" emptyQuery routes =! Some 2
    value POST "/some/path" emptyQuery routes =! Some 3

[<Fact>]
let ``Router Executes Pipeline Registered First`` () =
    let routes =
        freyaRouter {
            route GET "/" route1
            route All "/" route2 }

    value GET "/" emptyQuery routes =! Some 1

[<Fact>]
let ``Router Executes First Full Match`` () =
    let routes =
        freyaRouter {
            route All "/{one}/a" route1
            route All "/{two}/b" route2
            route All "/{one}/b" route3 }

    value GET "/some/b" emptyQuery routes =! Some 2