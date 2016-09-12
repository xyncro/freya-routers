namespace Freya.Routers.Uri.Template

open Freya.Core
open Freya.Core.Operators

// Types

// Types for the UriTemplateRouter, defined late to be extended and support
// static inference.

/// The core UriTemplateRouter type, used to give a concrete type rather than
/// a naked function, to allow for static member methods, and thus the
/// ability to take part in statically resolved type inference.

type UriTemplateRouter =
    | UriTemplateRouter of (UriTemplateRoutes -> unit * UriTemplateRoutes)

// UriTemplateRouter

// Functions defined against UriTemplateRouter, chiefly the simple monadic init
// and bind functions to enable the creation of a computation expression
// builder, plus a function to convert a router to a Pipeline, which will be
// used when defining the appropriate static members to allow the type to take
// part in static inference to either Freya<_> or Pipeline functions.

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module UriTemplateRouter =

    // Common

    let init _ : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            (), c)

    let bind (m: UriTemplateRouter, f: unit -> UriTemplateRouter) : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            let (UriTemplateRouter m) = m
            let (UriTemplateRouter f) = f ()

            (), snd (f (snd (m c))))

    // Custom

    let map (m: UriTemplateRouter, f: UriTemplateRoutes -> UriTemplateRoutes) : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            let (UriTemplateRouter m) = m

            (), f (snd (m c)))

    // Pipeline

    // Reification of a UriTemplateRouter instance to a pipeline, compiling
    // a generated set of routes and returning a function to evaluate the
    // result appropriately.

    let internal pipeline (UriTemplateRouter router) : Pipeline =
        let (_, UriTemplateRoutes routes) = router (UriTemplateRoutes [])
        let compilation = compile routes

        evaluate compilation >>=
            function | Some (data, pipeline) -> (Route.data_ .= Some data) *> pipeline
                     | _ -> Pipeline.next

// Extensions

// Implementations of the Freya and Pipeline typeclass-like static
// inference members defined in Freya.Core.

type UriTemplateRouter with

    /// A static member allowing UriTemplateRouter to be statically resolved as
    /// a Freya function, calling UriTemplateRouter.pipeline to construct the
    /// Freya function from the UriTemplateRouter instance.

    static member Freya router : Freya<_> =
        UriTemplateRouter.pipeline router

    /// A static member allowing UriTemplateRouter to be statically resolved as
    /// a Pipeline function, calling UriTemplateRouter.pipeline to construct
    /// the Pipeline function from the UriTemplateRouter instance.

    static member Pipeline router : Pipeline =
        UriTemplateRouter.pipeline router

