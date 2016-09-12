namespace Freya.Routers.Uri.Template

open Aether
open Aether.Operators
open Freya.Core
open Freya.Types.Http
open Freya.Types.Uri.Template

// Types

// Types for the basic definition of a matchable route, using URI Templates and
// where a matched route includes a pipeline to be run on a match, and which is
// returned as the result of router execution. *)

/// A collection of routes to be provided to a URI Template router.

type UriTemplateRoutes =
    | UriTemplateRoutes of UriTemplateRoute list

    static member routes_ =
        (fun (UriTemplateRoutes x) -> x), (UriTemplateRoutes)

/// A route to be handled by a URI Template router, including the method
/// specification, the template determining the path/query to match, and the
/// Pipeline to be run when the route is successfully matched.

 and UriTemplateRoute =
    { Method: UriTemplateRouteMethod
      Template: UriTemplate
      Pipeline: Pipeline }

    static member template_ =
        (fun x -> x.Template), (fun t x -> { x with Template = t })

/// The specification of the method semantics for URI Template route matching,
/// where a route may be specified to match All (any) methods, or a specific
/// list of methods.

 and UriTemplateRouteMethod =
    | All
    | Methods of Method list

// Route

/// Optic based access to data matched as part of routing, based on the basic
/// data forms defined as part of the URI Template RFC. The basic types
/// returned are simple core F# primitives. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Route =

    /// A lens from State -> UriTemplateData option, accessing the instance
    /// of template data which is set by a URI Template Router.

    let data_ =
            State.value_<UriTemplateData> "freya.routers.uri.template.data"

    /// A prism from State -> UriTemplate value given a key corresponding to a
    /// named expression in the matched URI Template. In most cases the more
    /// accurately typed atom_, list_, or keys_ prisms would be used instead
    /// to more succinctly access the underlying value.

    let value_ key =
            State.key_<UriTemplateData> "freya.routers.uri.template.data"
        >?> UriTemplateData.uriTemplateData_
        >?> Map.key_ (Key key)

    /// A prism from State -> string given a key corresponding to a named
    /// expression the matched URI Template. The string value is the Atom
    /// case unwrapped.

    let atom_ key =
            value_ key
        >?> UriTemplateValue.atom_

    /// A prism from State -> string list given a key corresponding to a named
    /// expression in the matched URI Template. The string list value is the
    /// List case unwrapped.

    let list_ key =
            value_ key
        >?> UriTemplateValue.list_

    /// A prism from State -> (string * string) list given a key corresponding
    /// to a named expression the matched URI Template. The (string * string)
    /// list value is the Keys case unwrapped.

    let keys_ key =
            value_ key
        >?> UriTemplateValue.keys_