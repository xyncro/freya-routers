namespace Freya.Routers.UriTemplate

open Aether
open Freya.Core

(* Expression

   A minimal computation expression for defining routers given ordered routes,
   defining a method and path specification, and a pipeline function to be
   returned when the route is the highest matched route under evaluation.
   
   The UriTemplateRouter type here would ordinarily be defined as part of the
   general set of types core to the system, but in this case must be defined
   late to be able to be augmented here due to the F# restriction on type
   extensions being present in the same file as the type declaration. *)

(* Types

   A simple computation expression builder type defined using the simple
   ConfigurationBuilder inheritance approach provided by Freya.Core.

   Includes extensions to UriRouterTemplateBuilder to define the custom
   route operation for declaring routes. *)

type UriTemplateRouterBuilder () =
    inherit ConfigurationBuilder<UriTemplateRouter>
        { Init = UriTemplateRouter.init
          Bind = UriTemplateRouter.bind }

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