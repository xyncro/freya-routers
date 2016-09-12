namespace Freya.Routers.Uri.Template

open Freya.Types.Http
open Freya.Types.Uri.Template

// Inference

[<AutoOpen>]
module Inference =

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module UriTemplateRouteMethod =

        // Inference

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

        /// Infers a UriTemplateRouteMethod from several possible types,
        /// including a raw UriTemplateRouteMethod (which will be returned
        /// unmodified), a Method list, which will be wrapped automatically in
        /// the UriTemplateRouteMethod type, and a single Method which will be
        /// wrapped in a list, and then the UriTemplateRouteMethod type.

        /// Any type which implements a suitable UriTemplateRouteMethod static
        /// member may also be inferred.

        let inline infer x =
            Inference.infer x

    [<RequireQualifiedAccess>]
    module UriTemplate =

        // Inference

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

        /// Infers a UriTemplate from either a UriTemplate which will be
        /// returned unmodified, or a string which will be parsed as a
        /// UriTemplate (with potential exceptions raised at runtime).

        /// Any type which implements a suitable UriTemplate static member may
        /// also be inferred.

        let inline infer x =
            Inference.infer x