namespace Freya.Routers.Uri.Template

open Aether
open Aether.Operators
open Anat.Operators
open Freya.Core
open Freya.Routers
open Freya.Types.Uri
open Freya.Types.Uri.Template

(* Types

   Types representing the elements of a compiled routing data structure, where
   expression based elements of a URI Template are modelled directly, but
   literal elememts are decompiled to a form of prefix trie, where the length
   of the prefix is taken to the next endpoint available. *)

type internal Route =
    | Route of Endpoint list * Target list

    static member endpoints_ =
        (fun (Route (es, _)) -> es), (fun es (Route (_, ts)) -> Route (es, ts))

    static member targets_ =
        (fun (Route (_, ts)) -> ts), (fun ts (Route (es, _)) -> Route (es, ts))

    static member empty =
            Route ([], [])

 and internal Endpoint =
    | Endpoint of int * UriTemplateRouteMethod * Pipeline

 and internal Target =
    | Expression of Expression
    | Trie of Trie

    static member expression_ =
        (function | Expression t -> Some t | _ -> None), (Expression)

    static member trie_ =
        (function | Trie l -> Some l | _ -> None), (Trie)

 and internal Expression =
    | Expression of Template.Expression * Route

 and internal Trie =
    | Trie of Map<string,Route> * int

(* Compilation

   Functions dealing with the compilation of a list of UriTemplateRoutes to a
   compiled Route which may then be evaluated as part of the general routing
   functionality. *)

[<AutoOpen>]
module internal Compilation =

    (* Optics

        Optics for accessing useful properties of the basic form of a route, a
        pair of an int and a UriTemplateRoute. These optics are used to
        simplify the interrogation and manipulation of state in the functions
        used to build and work with the Route instance. *)

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

    (* Routes

       A function to add int and UriTemplateRoute pairs to an existing Route,
       maintaining appropriate structure and precedence. *)

    // TODO: More complete commentary on these functions.

    let rec private routes (Route (es, ts)) =
            endpoints es &&& targets ts
        >>> Route

    (* Endpoints *)

    and private endpoints es =
            List.filter (Optic.get parts_ >> List.isEmpty)
        >>> List.map endpoint
        >>> List.append es

    and private endpoint (i, { Method = m; Pipeline = p }) =
            Endpoint (i, m, p)

    (* Targets *)

    and private targets ts =
            List.filter (Optic.get parts_ >> List.isEmpty >> not)
        >>> flip expressions &&& flip tries
        >>> List.pair
        >>> List.fold (flip apply) ts

    (* Expressions *)

    and private expressions ts =
            List.filter (Optic.get expression_ >> Option.isSome)
        >>> List.groupBy (Optic.get expression_ >> Option.get)
        >>> List.map (Optic.map snd_ (List.map (Optic.map parts_ List.tail)))
        >>> List.map expression
        >>> List.map Target.Expression
        >>> List.append ts

    and private expression (e, rs) =
            Expression (e, routes Route.empty rs)

    (* Tries *)

    and private tries ts =
            List.filter (Optic.get literal_ >> Option.isSome)
        >>> List.groupBy (Optic.get literal_ >> Option.get)
        >>> List.sortBy (Optic.get fst_ >> fun s -> s.Length)
        >>> List.map (Optic.map snd_ (List.map (Optic.map parts_ List.tail)))
        >>> List.fold (flip upsert) ts

    and private upsert krs =
            List.extract Target.trie_
        >>> function | [ t ], ts -> Target.Trie (update krs t) :: ts
                     | [   ], ts -> Target.Trie (insert krs) :: ts
                     | _ -> failwith ""

    and private update (k, rs) =
            function | Trie (m, s) when k.Length = s -> Trie (Optic.map (Map.value_ k) (eq rs) m, s)
                     | Trie (m, s) -> Trie (Optic.map (Map.value_ (k.Substring (0, s))) (gt (k.Substring (s)) rs) m, s)

    and private eq rs =
            function | Some r -> Some (routes r rs)
                     | _ -> Some (routes Route.empty rs)

    and private gt k rs =
            function | Some (Route (es, ts)) -> Some (Route (es, upsert (k, rs) ts))
                     | _ -> Some (Route ([], upsert (k, rs) []))

    and private insert (k, rs) =
            Trie (Map.ofList [ k, routes Route.empty rs ], k.Length)

    (* Compilation *)

    let compile =
            List.mapi pair
        >>> routes Route.empty