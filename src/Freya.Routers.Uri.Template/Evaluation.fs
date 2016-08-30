namespace Freya.Routers.Uri.Template

#nowarn "46"

open Aether
open Aether.Operators
open Anat.Operators
open FParsec
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http
open Freya.Polyfills
open Freya.Routers
open Freya.Types.Http
open Freya.Types.Uri.Template

(* Types

   The core types used in routing evaluation, namely for traversal state of the
   compiled graph and the return of a clear result to evaluation function. *)

(* Evaluation

   The result of routing evaluation, including matched data and the associated
   pipeline to run in the case where a route was matched. *)

type internal Evaluation =
    | Matched of UriTemplateData * Pipeline
    | Unmatched

(* Traversal

   State and carried data as part of the traversal of a compiled routing graph,
   including optics to provide lens based modification of the traversal while
   in progress. *)

type internal Traversal =
    | Traversal of Method * string * UriTemplateData

(* Request

   Optics and functions for working with the request to get a raw form path and
   query value, providing logical properties for the method and path and query
   of thre request as simple Freya typed functions. *)

[<RequireQualifiedAccess>]
module internal Request =

    (* Optics *)

    let private queryString_ =
            State.value_<string> "owin.RequestQueryString"
        >-> Option.unsafe_

    (* Utilities *)

    let private merge =
            function | path, query when query <> "" -> sprintf "%s?%s" path query
                     | path, _ -> path

    let private choose =
            function | Some pathRaw, _, query -> merge (pathRaw, query)
                     | _, path, query -> merge (path, query)

    (* Properties *)

    let method =
            !. Request.method_

    let pathAndQuery =
            fun pathRaw path query ->
                choose (pathRaw, path, query)
        <!> !. Request.pathRaw_
        <*> !. Request.path_
        <*> !. queryString_

(* Parsers

   Utility and pattern functions for working with parser cases within the
   routing target data structure. *)

[<AutoOpen>]
module internal Parsers =

    (* Utilities *)

    let private parse (s: string) =
        function | Success (d, _, p) -> Some (d, s.Substring (int p.Index))
                 | _ -> None

    (* Patterns *)

    let (|Parser|_|) =
        function | Target.Parser (Parser (e, r)) -> Some (e, r)
                 | _ -> None

    let (|Parse|_|) e =
        function | s -> parse s (run (Expression.Matching.Match e) s)

(* Tries

   Pattern functions for working with trie cases within the routing target data
   structure. *)

[<AutoOpen>]
module internal Tries =

    (* Patterns *)

    let (|Trie|_|) =
        function | Target.Trie (Trie (rs, l)) -> Some (rs, l)
                 | _ -> None

    let (|Split|_|) l =
        function | (s: string) when s.Length >= l -> Some (s.Substring (0, l), s.Substring (l))
                 | _ -> None

(* Search

   Search (traversal and selection) of a compiled structure, finding the
   matching routes (where present) and returning the route with the highest
   precedence, determined by the order that the route was specified in the
   original list of routes.

   The search is exhaustive and will find all matches, as it's needed to ensure
   that the order of the declarations is the deciding factor, not the sorted
   structure topology. *)

[<AutoOpen>]
module internal Search =

    (* Literals *)

    [<Literal>]
    let private E =
            ""

    (* Traverse *)

    let rec private traverse traversal =
            capture traversal &&& flip continue traversal
        >>> uncurry List.append

    (* Capture *)

    and private capture =
            function | Traversal (m, E, d) -> choose m d
                     | _ -> reject

    and private choose m d =
            function | Route (es, _) -> List.choose (handle m d) es

    and private handle m d =
            function | Endpoint (i, All, p) -> Some (i, d, p)
                     | Endpoint (i, Methods ms, p) when List.contains m ms -> Some (i, d, p)
                     | _ -> None

    and private reject =
            function | _ -> []

    (* Continue *)

    and private continue =
            function | Route (_, ts) -> target >> flip List.map ts >> List.concat

    and private target t =
            function | Parser (e, r) -> parser e r t
                     | Trie (rs, l) -> trie rs l t 
                     | _ -> []

    (* Parser *)

    and private parser e r =
            function | Traversal (m, Parse e (d', s), d) -> traverse (Traversal (m, s, d + d')) r
                     | _ -> []

    (* Trie *)

    and private trie rs l =
            function | Traversal (m, Split l (s1, s2), d) -> next m s2 d (Map.tryFind s1 rs)
                     | _ -> []

    and private next m s d =
            function | Some r -> traverse (Traversal (m, s, d)) r
                     | _ -> []

    (* Select *)

    let rec private select =
            function | [] -> Unmatched
                     | es -> Matched ((List.minBy order >> map) es)

    and private order =
            function | i, _, _ -> i

    and private map =
            function | _, d, p -> d, p

    (* Search *)

    let rec internal search route =
            flip traverse route >> select
        <!> init

    and private init =
            fun method pathAndQuery ->
                Traversal (method, pathAndQuery, UriTemplateData Map.empty)
        <!> Request.method
        <*> Request.pathAndQuery

(* Evaluation *)

[<AutoOpen>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Evaluation =

    (* Evaluation

       Run a search on the routing structure. Return an option of the data
       matched (if any) mapping from the internal Evaluation type. *)

    let evaluate route =
            function | Matched (data, pipe) -> Some (data, pipe)
                     | Unmatched -> None
        <!> search route