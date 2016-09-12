namespace Freya.Routers.Uri.Template

#nowarn "46"

open Aether
open Freya.Core

// Expression

// A minimal computation expression for defining routers given ordered routes,
// defining a method and path specification, and a pipeline function to be
// returned when the route is the highest matched route under evaluation.

// The UriTemplateRouter type here would ordinarily be defined as part of the
// general set of types core to the system, but in this case must be defined
// late to be able to be augmented here due to the F# restriction on type
// extensions being present in the same file as the type declaration.

// Types

// A simple computation expression builder type defined using the simple
// ConfigurationBuilder inheritance approach provided by Freya.Core.

// Includes extensions to UriRouterTemplateBuilder to define the custom
// route operation for declaring routes.

/// The type of computation expression builder used to provide the URI Template
/// Router computation expression syntax, based on the ConfigurationBuilder
/// defined by Freya Core.

type UriTemplateRouterBuilder () =
    inherit ConfigurationBuilder<UriTemplateRouter>
        { Init = UriTemplateRouter.init
          Bind = UriTemplateRouter.bind }

// Operations

type UriTemplateRouterBuilder with

    /// Register a route to be matched given a method specification, a URI
    /// Template to match the path and query, and a Pipeline to be executed
    /// upon successful matches.

    /// All values are statically inferred, see UriTemplateRouteMethod.infer,
    /// UriTemplate.infer and Pipeline.infer (Freya Core). This allows for
    /// simpler and more declarative configuration.

    [<CustomOperation ("route", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Route (m, method, template, pipeline) : UriTemplateRouter =
        UriTemplateRouter.map (m, Optic.map (Lens.ofIsomorphism UriTemplateRoutes.routes_) (fun r ->
            r @ [ { Method = UriTemplateRouteMethod.infer method
                    Template = UriTemplate.infer template
                    Pipeline = Pipeline.infer pipeline } ]))

    /// Register a resource to be matched given a URI Template to match the
    /// path and query, and a Pipeline to be executed upon successful matches.
    /// Resources are equivalent to routes where the method specification is
    /// All.

    /// All values are statically inferred, see UriTemplateRouteMethod.infer,
    /// UriTemplate.infer and Pipeline.infer (Freya Core). This allows for
    /// simpler and more declarative configuration.

    [<CustomOperation ("resource", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Resource (m, template, pipeline) : UriTemplateRouter =
        UriTemplateRouter.map (m, Optic.map (Lens.ofIsomorphism UriTemplateRoutes.routes_) (fun r ->
            r @ [ { Method = All
                    Template = UriTemplate.infer template
                    Pipeline = Pipeline.infer pipeline } ]))

// Builder

// The instance of UriTemplateRouterBuilder which will be used to provide the
// custom computation expression syntax. The instance is aliased as freyaRouter
// for backwards compatibility with earlier versions of Freya which assumed a
// single router implementation in perpetuity.

[<AutoOpen>]
module Builder =

    /// The Freya URI Template Route Builder computation expression, allowing
    /// a simple declarative syntax for configuring URI Template Routers.

    let freyaUriTemplateRouter =
        UriTemplateRouterBuilder ()

    /// A concise alias for freyaUriTemplateRouter for scopes where no other
    /// router may conflict in scope, allowing more concise syntax.

    let freyaRouter =
        freyaUriTemplateRouter