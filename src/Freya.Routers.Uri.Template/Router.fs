namespace Freya.Routers.Uri.Template

open Aether
open Aether.Operators
open Freya.Core
open Freya.Types.Http
open Freya.Types.Uri.Template
open Freya.Routers

(* Types

   Types for the basic definition of a matchable route, using URI Templates and
   where a matched route includes a pipeline to be run on a match, and which is
   returned as the result of router execution. *)

type UriTemplateRoutes =
    | UriTemplateRoutes of UriTemplateRoute list

    static member routes_ =
        (fun (UriTemplateRoutes x) -> x), (UriTemplateRoutes)

 and UriTemplateRoute =
    { Method: UriTemplateRouteMethod
      Template: UriTemplate
      Pipeline: Pipeline }

 and UriTemplateRouteMethod =
    | All
    | Methods of Method list

(* Route

   Optic based access to data matched as part of routing, based on the basic
   data forms defined as part of the URI Template RFC. The basic types returned
   are simple core F# primitives. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Route =

    let data_ =
            State.value_ "freya.routers.uritemplate.data"
        >-> Option.unsafe_

    let value_ key =
            data_ 
        >-> UriTemplateData.uriTemplateData_
        >-> Map.key_ (Key key)

    let atom_ key =
            value_ key
        >?> UriTemplateValue.atom_

    let list_ key =
            value_ key
        >?> UriTemplateValue.list_

    let keys_ key =
            value_ key
        >?> UriTemplateValue.keys_