[<AutoOpen>]
module Freya.Routers.UriTemplate.Tests.Prelude

open System.Collections.Generic
open Aether
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http
open Freya.Routers.UriTemplate

(* Base Data

   Default data instances to initialize tests, commonly empty states for
   verifying expected application of functions to state. *)

let private environment () =
    Dictionary<string, obj> () :> IDictionary<string, obj>

let private meta () =
    { Memos = Map.empty }

let private state () =
    { Environment = environment ()
      Meta = meta () }

(* Access

   Functions for getting and setting a known value against the state for the
   purpose of verification. *)

let private lens : Lens<State,int option> =
    State.value_ "test"

let get =
    Optic.get lens

let set i =
        Freya.Optic.set lens i
     *> Pipeline.next

(* Utilities

   Runner and verification functions. *)

let run meth path query m =
    Async.RunSynchronously ((   Freya.Optic.set Request.method_ meth
                             *> Freya.Optic.set Request.path_ path
                             *> Freya.Optic.set Request.query_ query
                             *> UriTemplateRouter.Pipeline m) (state ()))

let result meth path query m =
    fst (run meth path query m)

let value meth path query m =
    get (snd (run meth path query m))