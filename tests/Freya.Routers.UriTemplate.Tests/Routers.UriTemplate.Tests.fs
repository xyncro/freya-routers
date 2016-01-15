module Freya.Routers.UriTemplate.Tests

open System.Collections.Generic
open Aether
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http
open Freya.Routers.UriTemplate
open Swensen.Unquote
open Xunit

(* Prelude *)

[<AutoOpen>]
module internal Prelude =

    let private environment () =
        Dictionary<string, obj> () :> IDictionary<string, obj>

    let private meta () =
        { Memos = Map.empty }

    let private state () =
        { Environment = environment ()
          Meta = meta () }

    let get =
        Optic.get (State.value_ "test")

    let set i =
        Freya.Optic.set (State.value_ "test") i *> Pipeline.next

    let run meth path query m =
        Async.RunSynchronously ((   Freya.Optic.set Request.method_ meth
                                 *> Freya.Optic.set Request.path_ path
                                 *> Freya.Optic.set Request.query_ query
                                 *> UriTemplateRouter.Pipeline m) (state ()))

    let result meth path query m =
        fst (run meth path query m)

    let value meth path query m =
        get (snd (run meth path query m))

(* Mocks *)

[<AutoOpen>]
module internal Examples =

    let route1 =
        set (Some 1)

    let route2 =
        set (Some 2)

    let route3 =
        set (Some 3)

    let emptyQuery =
        Arachne.Uri.Query ""

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

    value GET "/" emptyQuery routes =! Some 1

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