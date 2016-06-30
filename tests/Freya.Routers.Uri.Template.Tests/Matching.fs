[<AutoOpen>]
module Freya.Routers.Uri.Template.Tests.Matching

open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http
open Freya.Routers.Uri.Template
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Xunit

(* Matching

   Tests covering basic matching of routes to paths and methods. Capture of
   matched variables is tested explicitly elsewhere. *)

[<Fact>]
let ``Router With Root Route Executes Correctly`` () =

    let router =
        freyaRouter {
            route All "/" (defaultValue .= Some "test") }

    verify defaultSetup router [
        defaultValue => Some "test" ]

[<Fact>]
let ``Router With Multiple Routes Executes Correctly`` () =

    let router =
        freyaRouter {
            route All "/" (defaultValue .= Some "one")
            route All "/some/path" (defaultValue .= Some "two")
            route All "/other/path" (defaultValue .= Some "three") }

    verify (Request.path_ .= "/") router [
        defaultValue => Some "one" ]

    verify (Request.path_ .= "/some/path") router [
        defaultValue => Some "two" ]

    verify (Request.path_ .= "/other/path") router [
        defaultValue => Some "three" ]

[<Fact>]
let ``Router With Method Specific Routes Executes Correctly`` () =

    let router =
        freyaRouter {
            route GET "/" (defaultValue .= Some "one")
            route GET "/some/path" (defaultValue .= Some "two")
            route POST "/some/path" (defaultValue .= Some "three") }

    verify (Request.method_ .= GET) router [
        defaultValue => Some "one" ]

    verify (Request.method_ .= POST) router [
        defaultValue => None ]

    verify ((Request.method_ .= GET) *> (Request.path_ .= "/some/path")) router [
        defaultValue => Some "two" ]

    verify ((Request.method_ .= POST) *> (Request.path_ .= "/some/path")) router [
        defaultValue => Some "three" ]

[<Fact>]
let ``Router Executes Pipeline Registered First`` () =

    let router =
        freyaRouter {
            route GET "/" (defaultValue .= Some "one")
            route All "/" (defaultValue .= Some "two") }

    verify defaultSetup router [
        defaultValue => Some "one" ]

[<Fact>]
let ``Router Executes First Full Match`` () =

    let router =
        freyaRouter {
            route All "/{one}/a" (defaultValue .= Some "one")
            route All "/{two}/b" (defaultValue .= Some "two")
            route All "/{one}/b" (defaultValue .= Some "three") }

    // TODO: Move the tests for route capture to a distinct file and set of tests!

    verify (Request.path_ .= "/some/b") router [
        Route.atom_ "one" => None
        Route.atom_ "two" => Some "some"
        defaultValue => Some "two" ]