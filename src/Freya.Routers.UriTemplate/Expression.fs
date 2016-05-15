namespace Freya.Routers.UriTemplate

open Aether
open Freya.Core
open Freya.Core.Operators

(* Expression

   A minimal computation expression for defining routers given ordered routes,
   defining a method and path specification, and a pipeline function to be
   returned when the route is the highest matched route under evaluation. *)

(* Types

   The simple UriTemplateRouter type, defined as a union rather than a simple
   type alias as the type will have members defined to allow it to be used in
   static inference. *)

type UriTemplateRouter =
    | UriTemplateRouter of (UriTemplateRoutes -> unit * UriTemplateRoutes)

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

    (* Inference *)

    let pipeline (UriTemplateRouter router) : Pipeline =
        let (_, UriTemplateRoutes routes) = router (UriTemplateRoutes [])
        let compilation = compile routes

        evaluate compilation >>= (
            function | Some (data, pipeline) -> (Route.data_ .= data) *> pipeline
                     | _ -> Pipeline.next)

(* Types

   A simple computation expression builder type defined using the simple
   ConfigurationBuilder inheritance approach provided by Freya.Core. *)

type UriTemplateRouterBuilder () =
    inherit ConfigurationBuilder<UriTemplateRouter>
        { Init = UriTemplateRouter.init
          Bind = UriTemplateRouter.bind }

(* Type Extensions

   Type extensions to UriTemplateRouter to define the member functions required
   for static type inference of Freya<_> and Pipeline types as defined by
   Freya.Core, and extensions to UriRouterTemplateBuilder to define the custom
   route operation for declaring routes. *)

type UriTemplateRouter with

    static member Freya (x: UriTemplateRouter) : Freya<_> =
        UriTemplateRouter.pipeline x

    static member Pipeline (x: UriTemplateRouter) : Pipeline =
        UriTemplateRouter.pipeline x

type UriTemplateRouterBuilder with

    [<CustomOperation ("route", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Route (m, meth, template, pipeline) : UriTemplateRouter =
        UriTemplateRouter.map (m, Optic.map (Lens.ofIsomorphism UriTemplateRoutes.routes_) (fun r ->
            r @ [ { Predicate = Method (UriTemplateRouteMethod.infer meth)
                    Specification = Path
                    Template = UriTemplate.infer template
                    Pipeline = Pipeline.infer pipeline } ]))

(* Builder

   The instance of UriTemplateRouterBuilder which will be used to provide the
   custom computation expression syntax. The instance is aliased as freyaRouter
   for backwards compatibility with earlier versions of Freya which assumed a
   single router implementation in perpetuity. *)

[<AutoOpen>]
module Builder =

    let freyaUriTemplateRouter =
        UriTemplateRouterBuilder ()

    let freyaRouter =
        freyaUriTemplateRouter