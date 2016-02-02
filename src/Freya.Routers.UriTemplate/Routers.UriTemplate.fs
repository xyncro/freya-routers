module Freya.Routers.UriTemplate

open Aether
open Aether.Operators
open Arachne.Http
open Arachne.Uri
open Arachne.Uri.Template
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http
open Freya.Routers
open Hekate

// Documentation
// TODO: Refactoring in Graphs

(* Routing

   The core routing engine based on matching URI Template specified routes,
   including a method or set of methods to match. Route data once matched is
   stored within the Environment, and made accessible through a set of
   optics.

   The implementation is based on an exhaustive graph search, finding the
   highest precedence matching route. *)

(* Types

   Types for the basic definition of a matchable route, using URI Templates
   and where a matched route includes a pipeline to be run on a match, and
   which is returned as the result of router execution. *)

type UriTemplateRoute =
    { Predicate: UriTemplateRoutePredicate
      Specification: UriTemplateRouteSpecification
      Template: UriTemplate
      Pipeline: Pipeline }

 and UriTemplateRoutePredicate =
    | Method of UriTemplateRouteMethod
    | Custom of Freya<bool>

 and UriTemplateRouteMethod =
    | All
    | Methods of Method list

 and UriTemplateRouteSpecification =
    | Path
    | PathAndQuery

(* Route

   Optic based access to data matched as part of routing, based on the basic
   data forms defined as part of the URI Template RFC. The basic types returned
   are simple core F# primitives. *)

[<RequireQualifiedAccess>]
module Route =

    let data_ =
            State.value_ "freya.routers.uritemplate.data"
        >-> Option.unsafe_

    let value_ key =
            data_ 
        >-> UriTemplateData.uriTemplateData_
        >-> Map.value_ (Key key)

    let atom_ key =
            value_ key
        >-> Option.mapEpimorphism UriTemplateValue.atom_

    let list_ key =
            value_ key
        >-> Option.mapEpimorphism UriTemplateValue.list_

    let keys_ key =
            value_ key
        >-> Option.mapEpimorphism UriTemplateValue.keys_

(* Graphs

   Graphs are used as the processing mechanism for a set of routes, the routes
   being compiled in to a traversable n-ary tree based on URI Template
   components and then evaluated to find the set of matching leaves. *)

[<AutoOpen>]
module internal Graphs =

    (* Compilation

       Compilation of a list of URI Template Routes in to a graph of possible
       pipeline results, storing the precedence at each valid leaf (or
       intermediate node where that node is also a valid endpoint for a
       search). *)

    [<RequireQualifiedAccess>]
    module Compilation =

        (* Types

           Types representing the elements of a compiled routing graph,
           modelling each aspect of the graph as a restrictive sum type. *)

        type CompilationGraph =
            | Graph of Graph<CompilationKey, CompilationNode, CompilationEdge>

            static member graph_ =
                (fun (Graph g) -> g), (Graph)

         and CompilationKey =
            | Root
            | Key of string

         and CompilationNode =
            | Empty
            | Endpoints of CompilationEndpoint list

         and CompilationEndpoint =
            | Endpoint of int * UriTemplateRoutePredicate * Pipeline

         and CompilationEdge =
            | Edge of FParsec.Primitives.Parser<UriTemplateData, unit>

        (* Defaults

           Default values for common structures, in this case a default (empty)
           compilation graph for use as the basis in compilation. *)

        let private defaultCompilationGraph =
            Graph (Graph.create [ Root, Empty ] [])

        (* Optics *)

        let private compilationGraph_ =
            Lens.ofIsomorphism CompilationGraph.graph_

        (* Patterns

           Active patterns used to discriminate while compiling a route,
           distinguishing between a part of the underlying URI Template that
           forms an intermediate node within the complete route, and the final
           element which should be represented within the graph as an endpoint
           (a node which has a non-empty list of Endpoint types). *)

        let private (|Next|_|) =
            function | { Predicate = predicate
                         Specification = spec
                         Template = UriTemplate (part :: parts)
                         Pipeline = pipe } -> Some (part, { Predicate = predicate
                                                            Specification = spec
                                                            Template = UriTemplate (parts)
                                                            Pipeline = pipe })
                     | _ -> None

        let private (|Last|_|) =
            function | { Predicate = predicate
                         Template = UriTemplate ([ part ])
                         Pipeline = pipe } -> Some (predicate, part, pipe)
                     | _ -> None

        (* Modification

           Functions to modify aspects of the routing graph, chiefly to add
           routes to the graph (instances of FreyaRoute). A fairly simple
           recursion over the route, taking the head of the URI Template giving
           the route each time until exhausted. *)

        let private composeKeys k1 k2 =
            match k1, k2 with
            | Key s1, Key s2 -> Key (s1 + s2)
            | _, Key s2 -> Key s2
            | Key s1, _ -> Key s1
            | _ -> Root

        let private addNode key =
            Graph.Nodes.add (key, Empty)

        let private updateNode key precedence predicate pipe =
            Graph.Nodes.map (fun key' node ->
                match key = key' with
                | true ->
                    match node with
                    | Empty -> Endpoints [ Endpoint (precedence, predicate, pipe) ]
                    | Endpoints (endpoints) -> Endpoints (endpoints @ [ Endpoint (precedence, predicate, pipe) ])
                | _ ->
                    node)

        let private addEdge key1 key2 part graph =
            Graph.Edges.add (key1, key2,
                Edge (UriTemplatePart.Matching.Match part)) graph

        let rec private addRoute current graph (precedence, route) =
            match route with
            | Last (predicate, part, pipe) ->
                let node =
                    composeKeys current (Key (string part))

                let graph =
                    ((fun graph ->
                        (match Graph.Nodes.contains node graph with
                         | false -> addNode node >> updateNode node precedence predicate pipe >> addEdge current node part
                         | _ -> updateNode node precedence predicate pipe) graph) ^% compilationGraph_) graph

                graph
            | Next (part, route) ->
                let node =
                    composeKeys current (Key (string part))

                let graph =
                    ((fun graph ->
                        (match Graph.Nodes.contains node graph with
                         | false -> addNode node >> addEdge current node part
                         | _ -> id) graph) ^% compilationGraph_) graph

                addRoute node graph (precedence, route)
            | _ ->
                graph

        (* Compilation

           A function to compile a list of raw FreyaRoute instances to
           an instance of a CompilationGraph, which can be executed
           directly (and hopefully efficiently). *)

        let compile =
                List.mapi (fun precedence route -> precedence, route)
             >> List.fold (addRoute Root) defaultCompilationGraph

    (* Evaluation
    
       Evaluation of a compiled graph, finding the matching routes (where
       present) and returning the route with the highest precedence, determined
       by the order that the route was specified in the original list of routes.

       The search is exhaustive and will find all matches, as it's needed to
       ensure that the order of the declarations is the deciding factor, not
       the sorted graph topology. *)

    [<RequireQualifiedAccess>]
    module Evaluation =

        type private EvaluationResult =
            | Matched of UriTemplateData * Pipeline
            | Unmatched

        (* Traversal *)

        type private Traversal =
            | Traversal of TraversalInvariant * TraversalState

            static member state_ =
                (fun (Traversal (_, s)) -> s), (fun s (Traversal (i, _)) -> Traversal (i, s))

         and private TraversalInvariant =
            | Invariant of Method

         and private TraversalState =
            | State of TraversalPosition * TraversalData

            static member position_ =
                (fun (State (p, _)) -> p), (fun p (State (_, d)) -> State (p, d))

            static member data_ =
                (fun (State (_, d)) -> d), (fun d (State (p, _)) -> State (p, d))

         and private TraversalPosition =
            | Position of string * Compilation.CompilationKey

            static member pathAndQuery_ =
                (fun (Position (p, _)) -> p), (fun p (Position (_, k)) -> Position (p, k))

            static member key_ =
                (fun (Position (_, k)) -> k), (fun k (Position (p, _)) -> Position (p, k))

         and private TraversalData =
            | Data of UriTemplateData

            static member data_ =
                (fun (Data d) -> d), (fun d (Data (_)) -> Data (d))

        (* Constructors

           Construction functions for common types, in this case a simple default
           traversal, starting at the tree root and capturing no data, with
           a starting path, and an invariant method on which to match. *)

        let private traversal meth path query =
            let pathAndQuery =
                match query with
                | "" -> path
                | query -> sprintf "%s?%s" path query

            Traversal (
                Invariant meth,
                State (
                    Position (pathAndQuery, Compilation.Root),
                    Data (UriTemplateData (Map.empty))))

        (* Optics

           Optics in to aspects of the traversal, chiefly the traversal state
           elements, taking an immutable approach to descending state capture
           throughout the graph traversal. *)

        let private traversalData_ =
                Traversal.state_
            >-> TraversalState.data_
            >-> TraversalData.data_

        let private traversalKey_ =
                Traversal.state_
            >-> TraversalState.position_
            >-> TraversalPosition.key_

        let private traversalPathAndQuery_ =
                Traversal.state_
            >-> TraversalState.position_
            >-> TraversalPosition.pathAndQuery_

        (* Patterns

           Patterns used to match varying states throughout the traversal
           process, beginning with the high level states that a traversal may
           occupy, i.e. working with a candidate match (when the path is
           exhausted) or working with a progression (the continuation of the
           current traversal).

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

        let private (|Successors|_|) key (Compilation.Graph graph) =
            match Graph.Nodes.successors key graph with
            | Some x -> Some x
            | _ -> None

        (* Matching *)

        let private (|Match|_|) parser pathAndQuery =
            match FParsec.CharParsers.run parser pathAndQuery with
            | FParsec.CharParsers.Success (data, _, p) -> Some (data, pathAndQuery.Substring (int p.Index))
            | _ -> None

        (* Filtering *)

        let private (|Endpoints|_|) key meth (Compilation.Graph graph) =
            match Graph.Nodes.tryFind key graph with
            | Some (_, Compilation.Endpoints endpoints) ->
                endpoints
                |> List.filter (
                   function | Compilation.Endpoint (_, Method (All), _) -> true
                            | Compilation.Endpoint (_, Method (Methods ms), _) when List.exists ((=) meth) ms -> true
                            | _ -> false)
                |> function | [] -> None
                            | endpoints -> Some endpoints
            | _ -> None

        (* Traversal

           Traversal of the compiled routing graph, finding all matches for the
           path and method in the traversal state. The search is exhaustive, as
           a search which only finds the first match may not find the match
           which has the highest declared precendence.

           The exhaustive approach also allows for potential secondary
           selection strategies in addition to simple precedence selection in
           future. *)

        let private emptyM =
            Freya.init []

        let private foldM f xs state =
            List.foldBack (fun x (xs, state) ->
                Async.RunSynchronously (f x state) ||> fun x state ->
                    (x :: xs, state)) xs ([], state)

        let private  mapM f xs =
                foldM f xs <!> Freya.State.get
            >>= fun (xs, state) ->
                        Freya.State.set state
                     *> Freya.init xs

        let rec private traverse graph traversal =
            match traversal with
            | Candidate (key, meth, data) ->
                match graph with
                | Endpoints key meth endpoints ->
                    Freya.init (
                        endpoints
                        |> List.map (fun (Compilation.Endpoint (precedence, _, pipe)) ->
                            precedence, data, pipe))
                | _ ->
                    emptyM
            | Progression (key, pathAndQuery) ->
                match graph with
                | Successors (key) successors ->
                        List.concat
                    <!> mapM (fun (key', Compilation.Edge parser) ->
                        match pathAndQuery with
                        | Match parser (data', pathAndQuery') ->
                            traversal
                            |> Optic.map traversalData_ ((+) data')
                            |> Optic.set traversalPathAndQuery_ pathAndQuery'
                            |> Optic.set traversalKey_ key'
                            |> traverse graph
                        | _ ->
                            emptyM) successors
                | _ -> emptyM
            | _ -> emptyM

        (* Selection

           Select the highest precedence data and pipeline pair from the given
           set of candidates, using the supplied precedence value. *)

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
           highest precedence, as measured by the order in which the routes
           were declared in the compilation phase. *)

        let private search graph =
                traversal <!> !. Request.method_ <*> !. Request.path_ <*> !. (Request.query_ >-> Query.raw_)
            >>= traverse graph
            >>= select

        (* Evaluation

           Run a search on the routing graph. In the case of a match, write
           any captured data to the state to be interrogated later through
           the routing lenses, and return the value of executing the matched
           pipeline.

           In the case of a non-match, fall through to whatever follows the
           router instance. *)

        let evaluate graph =
                search graph
            >>= function | Matched (data, pipe) -> (Route.data_ .= data) *> pipe
                         | Unmatched -> Pipeline.next

    (* Reification

       The process of reifying a set of routes in to a graph with evaluation
       pending, forming a Pipeline. *)

    [<RequireQualifiedAccess>]
    module Reification =

        let reify routes =
            Evaluation.evaluate (Compilation.compile routes)

(* Configuration

   Types and functions for user configuration of a URI Template Router, based
   on configuration through a computation expression with various custom
   operations, and taking advantage of static type inference to allow a concise
   and flexible API (for example, accepting a Method or list of Methods
   transparently). *)

(* Types

   The basic types of configuration, the Router (configuration) function, and
   the actual configuration data threaded through configuration functions.

   The function itself is defined as a single case discriminated union so that
   it can have static members, allowing it to take part in the static inference
   approaches of the basic Freya function, and Pipelines. *)

type UriTemplateRouter =
    | UriTemplateRouter of (UriTemplateRoutes -> unit * UriTemplateRoutes)

    static member Freya (UriTemplateRouter c) : Freya<_> =
        Graphs.Reification.reify (Optic.get
            (Lens.ofIsomorphism UriTemplateRoutes.routes_)
            (snd (c (UriTemplateRoutes []))))

    static member Pipeline (UriTemplateRouter c) : Pipeline =
        UriTemplateRouter.Freya (UriTemplateRouter c)

 and UriTemplateRoutes =
    | UriTemplateRoutes of UriTemplateRoute list

    static member routes_ =
        (fun (UriTemplateRoutes x) -> x), (UriTemplateRoutes)

(* Functions

   Functions implementing common monadic operations on the Router type,
   which will be used to implement the computation expression builder.

   Additionally some extensions to the basic requirements for a computation
   expression are included, including mapping over the inner state. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module UriTemplateRouter =

    (* Common *)

    let init _ : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            (), c)

    let bind (m: UriTemplateRouter, f: unit -> UriTemplateRouter) : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            let (UriTemplateRouter m) = m
            let (UriTemplateRouter f) = f ()

            (), snd (f (snd (m c))))

    (* Custom *)

    let inline map (m: UriTemplateRouter, f: UriTemplateRoutes -> UriTemplateRoutes) : UriTemplateRouter =
        UriTemplateRouter (fun c ->
            let (UriTemplateRouter m) = m

            (), f (snd (m c)))

    (* Operations *)

    let operations =
        { Configuration.Operations.Init = init
          Configuration.Operations.Bind = bind }

(* Inference

   Pseudo-Typeclass style static inference for Methods and for URI Templates,
   allowing people to use more concise notation within the configuring
   computation expression, as well as potentially implementing new types which
   can be used in the computation expression by defining the appropriate type
   level members. *)

[<RequireQualifiedAccess>]
module Infer =

    (* URI Template *)

    [<RequireQualifiedAccess>]
    module UriTemplate =

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

    let inline uriTemplate x =
        UriTemplate.infer x

    (* URI Template Route Method *)

    [<RequireQualifiedAccess>]
    module UriTemplateRouteMethod =

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

    let inline uriTemplateRouteMethod x =
        UriTemplateRouteMethod.infer x

(* Builder

   Computation expression builder for configuring the URI Template Router,
   providing a simple type-safe syntax and static inference based overloads of
   single functions. *)

type UriTemplateRouterBuilder () =
    inherit Configuration.Builder<UriTemplateRouter> (UriTemplateRouter.operations)

(* Syntax *)

type UriTemplateRouterBuilder with

    [<CustomOperation ("route", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Route (m, meth, template, pipeline) : UriTemplateRouter =
        UriTemplateRouter.map (m, Optic.map (Lens.ofIsomorphism UriTemplateRoutes.routes_) (fun r ->
            r @ [ { Predicate = Method (Infer.uriTemplateRouteMethod meth)
                    Specification = Path
                    Template = Infer.uriTemplate template
                    Pipeline = Infer.pipeline pipeline } ]))

(* Expressions

   Computation expressions, instances of the configuration builder. The fully
   named instance, freyaUriTemplateRouter is aliased to freyaRouter to provide
   the possibility of more concise code when only one approach to routing is in
   scope.

   This naming also matches the original single form approach to routing, and
   provides backwards compatibility. *)

let freyaUriTemplateRouter =
    UriTemplateRouterBuilder ()

let freyaRouter =
    freyaUriTemplateRouter