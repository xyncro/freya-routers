namespace Freya.Routers.UriTemplate

open Aether
open Aether.Operators
open Arachne.Http
open Arachne.Uri.Template
open Freya.Core
open Freya.Routers

(* Types

   Types for the basic definition of a matchable route, using URI Templates and
   where a matched route includes a pipeline to be run on a match, and which is
   returned as the result of router execution. *)

type UriTemplateRouter =
    | UriTemplateRouter of (UriTemplateRoutes -> unit * UriTemplateRoutes)

 and UriTemplateRoutes =
    | UriTemplateRoutes of UriTemplateRoute list

    static member routes_ =
        (fun (UriTemplateRoutes x) -> x), (UriTemplateRoutes)

 and UriTemplateRoute =
    { Predicate: UriTemplateRoutePredicate
      Specification: UriTemplateRouteSpecification
      Template: UriTemplate
      Pipeline: Pipeline }

 and UriTemplateRoutePredicate =
    | Method of UriTemplateRouteMethod
    | Custom of Freya<bool>

 and UriTemplateRouteMethod =
    | All
    | Methods of Method list

 and UriTemplateRouteSpecification =
    | Path
    | PathAndQuery

(* UriTemplateRouter

   Functions defined against UriTemplateRouter, chiefly the simple monadic init
   and bind functions to enable the creation of a computation expression
   builder, plus a function to convert a router to a Pipeline, which will be
   used when defining the appropriate static members to allow the type to take
   part in static inference to either Freya<_> or Pipeline functions. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module UriTemplateRouter =

    (* Common *)

    let init _ : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            (), c)

    let bind (m: UriTemplateRouter, f: unit -> UriTemplateRouter) : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            let (UriTemplateRouter m) = m
            let (UriTemplateRouter f) = f ()

            (), snd (f (snd (m c))))

    (* Custom *)

    let map (m: UriTemplateRouter, f: UriTemplateRoutes -> UriTemplateRoutes) : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            let (UriTemplateRouter m) = m

            (), f (snd (m c)))

(* UriTemplateRouteMethod

   Functions for working with UriTemplateRouteMethod instances, in this case
   providing static inference capabilities to allow for more forgiving APIs
   when providing Methods (giving single, multiple etc. as equivalent
   arguments). *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module UriTemplateRouteMethod =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member inline UriTemplateRouteMethod (x: UriTemplateRouteMethod) =
                x

            static member inline UriTemplateRouteMethod (x: Method list) =
                Methods x

            static member inline UriTemplateRouteMethod (x: Method) =
                Methods [ x ]

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member UriTemplateRouteMethod: ^a -> UriTemplateRouteMethod) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer x =
        Inference.infer x

(* UriTemplate

   Functions for working with UriTemplate instances, in this case allowing for
   static type inference to be used to define APIs where UriTemplates and
   strings may be used interchangeably (though with less compile time safety as
   the parse may fail at runtime). *)

[<RequireQualifiedAccess>]
module UriTemplate =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member inline UriTemplate (x: UriTemplate) =
                x

            static member inline UriTemplate (x: string) =
                UriTemplate.parse x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member UriTemplate: ^a -> UriTemplate) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer x =
        Inference.infer x

(* Route

   Optic based access to data matched as part of routing, based on the basic
   data forms defined as part of the URI Template RFC. The basic types returned
   are simple core F# primitives. *)

[<RequireQualifiedAccess>]
module Route =

    let data_ =
            State.value_ "freya.routers.uritemplate.data"
        >-> Option.unsafe_

    let value_ key =
            data_ 
        >-> UriTemplateData.uriTemplateData_
        >-> Map.value_ (Key key)

    let atom_ key =
            value_ key
        >-> Option.mapEpimorphism UriTemplateValue.atom_

    let list_ key =
            value_ key
        >-> Option.mapEpimorphism UriTemplateValue.list_

    let keys_ key =
            value_ key
        >-> Option.mapEpimorphism UriTemplateValue.keys_