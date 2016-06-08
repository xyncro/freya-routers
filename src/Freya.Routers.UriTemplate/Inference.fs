namespace Freya.Routers.UriTemplate

open Arachne.Http
open Arachne.Uri.Template

(* Inference *)

[<AutoOpen>]
module Inference =

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