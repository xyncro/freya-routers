namespace Freya.Routers.Uri.Template

open Aether
open Aether.Operators
open FParsec
open Freya.Core
open Freya.Types.Uri.Template
open Hekate

(* Types

   Types representing the elements of a compiled routing graph, modelling each
   aspect of the graph as a restrictive sum type. *)

type internal Compilation =
    | Compilation of Graph<Key, Node, Edge>

    static member compilation_ =
        (fun (Compilation g) -> g), (Compilation)

 and internal Key =
    | Root
    | Key of string

 and internal Node =
    | Empty
    | Endpoints of Endpoint list

 and internal Endpoint =
    | Endpoint of int * UriTemplateRoutePredicate * Pipeline

 and internal Edge =
    | Edge of Parser<UriTemplateData, unit>

(* Compilation

   Functions dealing with the compilation of a list of UriTemplateRoutes to a
   compiled routing graph which may be evaluated as part of the routing
   functionality. *)

[<AutoOpen>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Compilation =

    (* Defaults

       Default values for common structures, in this case a default (empty)
       compilation graph for use as the basis in compilation. *)

    let private defaultCompilation =
        Compilation (Graph.create [ Root, Empty ] [])

    (* Optics *)

    let private compilation_ =
        Lens.ofIsomorphism Compilation.compilation_

    (* Patterns

       Active patterns used to discriminate while compiling a route,
       distinguishing between a part of the underlying URI Template that forms
       an intermediate node within the complete route, and the final element
       which should be represented within the graph as an endpoint (a node
       which has a non-empty list of Endpoint types). *)

    let private (|Next|_|) =
        function | { Predicate = predicate
                     Template = UriTemplate (part :: parts)
                     Pipeline = pipe } -> Some (part, { Predicate = predicate
                                                        Template = UriTemplate (parts)
                                                        Pipeline = pipe })
                 | _ -> None

    let private (|Last|_|) =
        function | { Predicate = predicate
                     Template = UriTemplate ([ part ])
                     Pipeline = pipe } -> Some (predicate, part, pipe)
                 | _ -> None

    (* Modification

       Functions to modify aspects of the routing graph, chiefly to add routes
       to the graph (instances of FreyaRoute). A fairly simple recursion over
       the route, taking the head of the URI Template giving the route each
       time until exhausted. *)

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
                     | _ -> updateNode node precedence predicate pipe) graph) ^% compilation_) graph

            graph
        | Next (part, route) ->
            let node =
                composeKeys current (Key (string part))

            let graph =
                ((fun graph ->
                    (match Graph.Nodes.contains node graph with
                     | false -> addNode node >> addEdge current node part
                     | _ -> id) graph) ^% compilation_) graph

            addRoute node graph (precedence, route)
        | _ ->
            graph

    (* Compilation

       A function to compile a list of raw FreyaRoute instances to an instance
       of a Compilation, which can be executed directly (and hopefully
       efficiently). *)

    let compile =
            List.mapi (fun precedence route -> precedence, route)
         >> List.fold (addRoute Root) defaultCompilation
