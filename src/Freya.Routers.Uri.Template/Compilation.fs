namespace Freya.Routers.Uri.Template

#nowarn "46"

open FParsec
open Freya.Core
open Freya.Types.Uri.Template
open Hekate

(* Types

   Types representing the elements of a compiled routing trie, modelling each
   aspect of the trie as a restrictive sum type. *)

type internal Route =
    | Route of Endpoint list * Remainder list

 and internal Endpoint =
    | Endpoint of int * UriTemplateRouteMethod * Pipeline

 and internal Remainder =
    | Remainder of Parser<UriTemplateData,unit> * Route

(* Construction

   Functions dealing with the compilation of a list of UriTemplateRoutes to a
   compiled routing graph which may be evaluated as part of the routing
   functionality. *)

[<AutoOpen>]
module internal Construction =

    (* Types *)

    type Key =
        | Root
        | Key of string

     and Node =
        | Empty
        | Endpoints of Endpoint list

     and Edge =
        | Edge of UriTemplatePart

    (* Defaults

       Default values for common structures, in this case a default (empty)
       compilation graph for use as the basis in compilation. *)

    let private defaultConstruction =
        Graph.create [ Root, Empty ] []

    (* Patterns

       Active patterns used to discriminate while compiling a route,
       distinguishing between a part of the underlying URI Template that forms
       an intermediate node within the complete route, and the final element
       which should be represented within the graph as an endpoint (a node
       which has a non-empty list of Endpoint types). *)

    let private (|Next|_|) =
        function | { Method = method
                     Template = UriTemplate (part :: parts)
                     Pipeline = pipe } -> Some (part, { Method = method
                                                        Template = UriTemplate (parts)
                                                        Pipeline = pipe })
                 | _ -> None

    let private (|Last|_|) =
        function | { Method = method
                     Template = UriTemplate ([ part ])
                     Pipeline = pipe } -> Some (method, part, pipe)
                 | _ -> None

    (* Modification

       Functions to modify aspects of the routing graph, chiefly to add routes
       to the graph (instances of FreyaRoute). A fairly simple recursion over
       the route, taking the head of the URI Template giving the route each
       time until exhausted. *)

    let private composeKeys key1 key2 =
        match key1, key2 with
        | Key s1, Key s2 -> Key (s1 + s2)
        | _, Key s2 -> Key s2
        | Key s1, _ -> Key s1
        | _ -> Root

    let private addNode key =
        Graph.Nodes.add (key, Empty)

    let private updateNode key precedence m part =
        Graph.Nodes.map (fun key' n ->
            match key = key' with
            | true ->
                match n with
                | Empty -> Endpoints [ Endpoint (precedence, m, part) ]
                | Endpoints (es) -> Endpoints (es @ [ Endpoint (precedence, m, part) ])
            | _ ->
                n)

    let private addEdge key1 key2 part graph =
        Graph.Edges.add (key1, key2, Edge part) graph

    let rec private addRoute key1 graph (precedence, route) =
        match route with
        | Last (method, part, pipe) ->
            let key2 =
                composeKeys key1 (Key (string part))

            let graph =
                ((fun graph ->
                    (match Graph.Nodes.contains key2 graph with
                     | false -> addNode key2 >> updateNode key2 precedence method pipe >> addEdge key1 key2 part
                     | _ -> updateNode key2 precedence method pipe)) graph) graph

            graph
        | Next (part, route) ->
            let key2 =
                composeKeys key1 (Key (string part))

            let graph =
                ((fun graph ->
                    (match Graph.Nodes.contains key2 graph with
                     | false -> addNode key2 >> addEdge key1 key2 part
                     | _ -> id)) graph) graph

            addRoute key2 graph (precedence, route)
        | _ ->
            graph

    let construct =
            List.mapi (fun precedence route -> precedence, route)
         >> List.fold (addRoute Root) defaultConstruction

(* Deconstruction *)

[<AutoOpen>]
module internal Deconstruction =

    let rec private route key graph =
        match Graph.Nodes.tryFind key graph, Graph.Nodes.outward key graph with
        | Some (_, Endpoints endpoints), Some nodes -> Route (endpoints, remainders graph nodes)
        | Some (_), Some nodes -> Route ([], remainders graph nodes)
        | _ -> Route ([],[])

    and private remainders graph =
        List.map (fun (_, key, Edge part) -> Remainder (UriTemplatePart.Matching.Match part, route key graph))

    let deconstruct =
        route Root

(* Compilation *)

[<AutoOpen>]
module internal Compilation =

    let compile =
            construct
         >> deconstruct