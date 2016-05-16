namespace Freya.Routers.UriTemplate

open Freya.Core
open Freya.Core.Operators

(* Extension

   Type extensions for the UriTemplateRouter to implement common static
   inference (typeclass-like) functionality defined as part of Freya.Core. *)

[<AutoOpen>]
module Extension =

    (* Functions

       Reification of a UriTemplateRouter instance to a pipeline, compiling
       a generated set of routes and returning a function to evaluate the
       result appropriately. *)

    let private extend (UriTemplateRouter router) : Pipeline =
        let (_, UriTemplateRoutes routes) = router (UriTemplateRoutes [])
        let compilation = compile routes

        evaluate compilation >>= (
            function | Some (data, pipeline) -> (Route.data_ .= data) *> pipeline
                     | _ -> Pipeline.next)

    (* Extensions

       Implementations of the Freya and Pipeline typeclass-like static
       inference members defined in Freya.Core. *)

    type UriTemplateRouter with

        static member Freya router : Freya<_> =
            extend router

        static member Pipeline router : Pipeline =
            extend router

