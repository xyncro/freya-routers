namespace Freya.Routers.UriTemplate

open Aether
open Aether.Operators
open Arachne.Http
open Arachne.Uri
open Arachne.Uri.Template
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http
open Hekate

(* Evaluation

   Evaluation of a compiled graph, finding the matching routes (where present)
   and returning the route with the highest precedence, determined by the order
   that the route was specified in the original list of routes.

   The search is exhaustive and will find all matches, as it's needed to ensure
   that the order of the declarations is the deciding factor, not the sorted
   graph topology. *)

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
    | Traversal of Invariant * State

    static member state_ =
        (fun (Traversal (_, s)) -> s), (fun s (Traversal (i, _)) -> Traversal (i, s))

 and internal Invariant =
    | Invariant of Method

 and internal State =
    | State of Position * Data

    static member position_ =
        (fun (State (p, _)) -> p), (fun p (State (_, d)) -> State (p, d))

    static member data_ =
        (fun (State (_, d)) -> d), (fun d (State (p, _)) -> State (p, d))

 and internal Position =
    | Position of string * Key

    static member pathAndQuery_ =
        (fun (Position (p, _)) -> p), (fun p (Position (_, k)) -> Position (p, k))

    static member key_ =
        (fun (Position (_, k)) -> k), (fun k (Position (p, _)) -> Position (p, k))

 and internal Data =
    | Data of UriTemplateData

    static member data_ =
        (fun (Data d) -> d), (fun d (Data (_)) -> Data (d))

(* Evaluation

   Functions for the evaluation of a compiled routing graph. *)

[<AutoOpen>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Evaluation =

    (* Constructors

        Construction functions for common types, in this case a simple default
        traversal, starting at the tree root and capturing no data, with a
        starting path, and an invariant method on which to match. *)

    let private traversal meth path query =
        let pathAndQuery =
            match query with
            | "" -> path
            | query -> sprintf "%s?%s" path query

        Traversal (
            Invariant meth,
            State (
                Position (pathAndQuery, Root),
                Data (UriTemplateData (Map.empty))))

    (* Optics

        Optics in to aspects of the traversal, chiefly the traversal state
        elements, taking an immutable approach to descending state capture
        throughout the graph traversal. *)

    let private data_ =
            Traversal.state_
        >-> State.data_
        >-> Data.data_

    let private traversalKey_ =
            Traversal.state_
        >-> State.position_
        >-> Position.key_

    let private traversalPathAndQuery_ =
            Traversal.state_
        >-> State.position_
        >-> Position.pathAndQuery_

    (* Patterns

        Patterns used to match varying states throughout the traversal process,
        beginning with the high level states that a traversal may occupy, i.e.
        working with a candidate match (when the path is exhausted) or working
        with a progression (the continuation of the current traversal).

        A pattern for matching paths against the current parser, with data
        captured and the result paths returned follows, before a filtering
        pattern to only return candidate endpoints which match the invariant
        method stored as part of the traversal. *)

    (* Traversal *)

    let private (|Candidate|_|) =
        function | Traversal (Invariant m, State (Position ("", k), Data d)) -> Some (k, m, d)
                 | _ -> None

    let private (|Progression|_|) =
        function | Traversal (Invariant _, State (Position (p, k), Data _)) -> Some (k, p)

    let private (|Successors|_|) key (Compilation graph) =
        match Graph.Nodes.successors key graph with
        | Some x -> Some x
        | _ -> None

    (* Matching *)

    let private (|Match|_|) parser pathAndQuery =
        match FParsec.CharParsers.run parser pathAndQuery with
        | FParsec.CharParsers.Success (data, _, p) -> Some (data, pathAndQuery.Substring (int p.Index))
        | _ -> None

    (* Filtering *)

    let private (|Endpoints|_|) key meth (Compilation graph) =
        match Graph.Nodes.tryFind key graph with
        | Some (_, Endpoints endpoints) ->
            endpoints
            |> List.filter (
               function | Endpoint (_, Method (All), _) -> true
                        | Endpoint (_, Method (Methods ms), _) when List.exists ((=) meth) ms -> true
                        | _ -> false)
            |> function | [] -> None
                        | endpoints -> Some endpoints
        | _ -> None

    (* Traversal

        Traversal of the compiled routing graph, finding all matches for the
        path and method in the traversal state. The search is exhaustive, as a
        search which only finds the first match may not find the match which
        has the highest declared precendence.

        The exhaustive approach also allows for potential secondary selection
        strategies in addition to simple precedence selection in future. *)

    let private emptyM =
        Freya.init []

    let private foldM f xs state =
        List.foldBack (fun x (xs, state) ->
            Async.RunSynchronously (f x state) ||> fun x state ->
                (x :: xs, state)) xs ([], state)

    let private  mapM f xs =
            foldM f xs <!> Freya.Optic.get id_
        >>= fun (xs, state) ->
                    Freya.Optic.set id_ state
                 *> Freya.init xs

    let rec private traverse graph traversal =
        match traversal with
        | Candidate (key, meth, data) ->
            match graph with
            | Endpoints key meth endpoints ->
                Freya.init (
                    endpoints
                    |> List.map (fun (Endpoint (precedence, _, pipe)) ->
                        precedence, data, pipe))
            | _ ->
                emptyM
        | Progression (key, pathAndQuery) ->
            match graph with
            | Successors (key) successors ->
                    List.concat
                <!> mapM (fun (key', Edge parser) ->
                    match pathAndQuery with
                    | Match parser (data', pathAndQuery') ->
                        traversal
                        |> Optic.map data_ ((+) data')
                        |> Optic.set traversalPathAndQuery_ pathAndQuery'
                        |> Optic.set traversalKey_ key'
                        |> traverse graph
                    | _ ->
                        emptyM) successors
            | _ -> emptyM
        | _ -> emptyM

    (* Selection

        Select the highest precedence data and pipeline pair from the given set
        of candidates, using the supplied precedence value. *)

    let private select =
        function | [] ->
                    Freya.init (
                        Unmatched)
                 | endpoints ->
                    Freya.init (
                        Matched (
                            endpoints
                            |> List.minBy (fun (precedence, _, _) -> precedence)
                            |> fun (_, data, pipe) -> data, pipe))

    (* Search

        Combine a list of all possible route matches and associated captured
        data, produced by a traversal of the compiled routing graph, with a
        selection of the matched route (data and pipeline pair) with the
        highest precedence, as measured by the order in which the routes were
        declared in the compilation phase. *)

    let private search graph =
            traversal <!> !. Request.method_ <*> !. Request.path_ <*> !. (Request.query_ >-> Query.raw_)
        >>= traverse graph
        >>= select

    (* Evaluation

        Run a search on the routing graph. In the case of a match, write any
        captured data to the state to be interrogated later through the routing
        lenses, and return the value of executing the matched pipeline.

        In the case of a non-match, fall through to whatever follows the router
        instance. *)

    let evaluate compilation =
            search compilation
        >>= function | Matched (data, pipe) -> Freya.init (Some (data, pipe))
                     | Unmatched -> Freya.init None