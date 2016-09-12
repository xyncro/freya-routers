namespace Freya.Routers.Uri.Template

#nowarn "46"

open Aether
open Aether.Operators
open Anat.Operators
open Freya.Core
open Freya.Routers
open Freya.Types.Uri.Template

// Types

// Types representing the elements of a compiled routing data structure, where
// expression based elements of a URI Template are modelled directly as parsers,
// but literal elememts are decompiled to a form of prefix trie, where the
// length of the prefix is taken to the next endpoint available.

type internal Route =
    | Route of Endpoint list * Target list

    static member empty =
        Route ([], [])

 and internal Endpoint =
    | Endpoint of int * UriTemplateRouteMethod * Pipeline

 and internal Target =
    | Parser of Parser
    | Trie of Trie

    static member trie_ =
        (function | Trie l -> Some l | _ -> None), (Trie)

 and internal Parser =
    | Parser of Expression * Route

 and internal Trie =
    | Trie of Map<string,Route> * int

// Compilation

// Functions dealing with the compilation of a list of UriTemplateRoutes to a
// compiled Route which may then be evaluated as part of the general routing
// functionality

[<AutoOpen>]
module internal Compilation =

    // Strings

    let private left (k: string) l =
        k.Substring (0, l)

    let private right (k: string) l =
        k.Substring (l)

    // Optics

    // Optics for accessing useful properties of the basic form of a route, a
    // pair of an int and a UriTemplateRoute. These optics are used to
    // simplify the interrogation and manipulation of state in the functions
    // used to build and work with the Route instance.

    let private template_ =
            snd_
        >-> UriTemplateRoute.template_

    let private parts_ =
            template_
        >-> UriTemplate.uriTemplate_

    let private expression_ =
            parts_
        >-> List.head_
        >?> UriTemplatePart.expression_

    let private literal_ =
            parts_
        >-> List.head_
        >?> UriTemplatePart.literal_
        >?> Literal.literal_

    // Routes

    // A function to add int and UriTemplateRoute pairs to an existing Route,
    // maintaining appropriate structure and precedence.

    // TODO: More complete commentary on these functions.

    let rec private routes (Route (es, ts)) =
            endpoints es &&& targets ts
        >>> Route

    // Endpoints

    and private endpoints es =
            List.filter (Optic.get parts_ >> List.isEmpty)
        >>> List.map endpoint
        >>> List.append es

    and private endpoint (i, { Method = m; Pipeline = p }) =
            Endpoint (i, m, p)

    // Targets

    and private targets ts =
            List.filter (Optic.get parts_ >> List.isEmpty >> not)
        >>> flip parsers &&& flip tries
        >>> List.pair
        >>> List.fold (flip apply) ts

    // Parsers

    and private parsers ts =
            List.filter (Optic.get expression_ >> Option.isSome)
        >>> List.groupBy (Optic.get expression_ >> Option.get)
        >>> List.map (Optic.map snd_ (List.map (Optic.map parts_ List.tail)))
        >>> List.map parser
        >>> List.map Target.Parser
        >>> List.append ts

    and private parser (e, irs) =
            Parser (e, routes Route.empty irs)

    // Tries

    and private tries ts =
            List.filter ((Optic.get literal_) >> Option.isSome)
        >>> List.groupBy (Optic.get literal_ >> Option.get)
        >>> List.sortBy (Optic.get fst_ >> fun literal -> literal.Length)
        >>> List.map (Optic.map snd_ (List.map (Optic.map parts_ List.tail)))
        >>> List.fold (flip upsert) ts

    and private upsert kirs =
            List.extract Target.trie_
        >>> function | [ t ], ts -> Target.Trie (update kirs t) :: ts
                     | [   ], ts -> Target.Trie (insert kirs) :: ts
                     | _ -> failwith ""

    and private update (k, irs) =
            function | Trie (rs, l) when k.Length = l -> Trie (Optic.map (Map.value_ k) (eq irs) rs, l)
                     | Trie (rs, l) -> Trie (Optic.map (Map.value_ (left k l)) (gt (right k l) irs) rs, l)

    and private eq irs =
            function | Some r -> Some (routes r irs)
                     | _ -> Some (routes Route.empty irs)

    and private gt k irs =
            function | Some (Route (es, ts)) -> Some (Route (es, upsert (k, irs) ts))
                     | _ -> Some (Route ([], upsert (k, irs) []))

    and private insert (k, irs) =
            Trie (Map.ofList [ k, routes Route.empty irs ], k.Length)

    // Compilation

    let compile =
            List.mapi pair
        >>> routes Route.empty