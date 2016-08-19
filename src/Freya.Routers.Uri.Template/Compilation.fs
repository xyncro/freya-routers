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


module TempLiterals =

    open Aether

    (* Literals *)

    let [<Literal>] private E =
        ""

    (* Types *)

    type Trie =
        | Trie of Map<string,(Endpoints * Trie)> * int
        | Empty

     and Endpoints =
        | Endpoints of int list

    (* Inclusion

       Functions for ensuring the inclusion of a string and endpoint pair within a
       Trie, either by adding to an existing Trie, or creating a Trie if the trie
       is currently Empty. *)

    let rec include =
        function | Trie (map, size) -> update map size
                 | Empty -> create

    (* Update

       Functions for creating a trie with a string and endpoint pair, given a map
       and size. If the string is empty, a Trie will be created with the unmodified
       map and size

       When the string is shorter than the current size, the map will be resized to
       reduce the key length to the length of the string, shifting the contents down
       one level within the trie structure, before the endpoint is added to the map
       resulting.

       When the string is longer than the current size, the remainder of the string
       will be added to the trie at the key given by the first part of the string if
       one exists, or to an Empty trie at that location if not, up to the current
       size.

       When the string matches the current size, the endpoint will be added 
       directly. *)

    and update m l =
        function | E, _ -> Trie (m, l)
                 | Split l (p: string, _), e when p.Length < l -> Trie (Optic.map (Map.value_ p) (add e) (resize p.Length m), p.Length)
                 | Split l (p, r: string), e when r.Length > 0 -> Trie (Optic.map (Map.value_ p) (continue e r) m, l)
                 | Split l (p, _), e -> Trie (Optic.map (Map.value_ p) (add e) m, l)

    and add e =
        function | Some (Endpoints es, t) -> Some (Endpoints (e :: es), t)
                 | _ -> Some (Endpoints [ e ], Empty)

    and continue e r =
        function | Some (es, t) -> Some (es, include t (r, e))
                 | _ -> Some (Endpoints [], include Empty (r, e))

    and resize l =
        function | map -> Map.fold (fun m (Split l (p, r)) v -> Optic.map (Map.value_ p) (shift r v) m) Map.empty map

    and shift k v =
        function | Some (es, Trie (map, s)) -> Some (es, Trie (Map.add k v map, s))
                 | _ -> Some (Endpoints [], Trie (Map.ofList [ k, v ], k.Length))

    (* Create

       Functions for creating a trie from a string and endpoint pair. If the string
       is empty, the trie will be Empty. If the string is a valid string, a new
       Trie will be created with the endpoint provided. *)

    and create =
        function | E, _ -> Empty
                 | s, e -> Trie (Map.ofList [ s, (Endpoints [ e ], Empty) ], s.Length)

    (* Strings *)

    and (|Split|) l =
        function | s -> prefix l s, remainder l s

    and prefix l =
        function | s when s.Length < l -> s
                 | s -> s.Substring (0, l)

    and remainder l =
        function | s when s.Length < l -> E
                 | s -> s.Substring (l)


module TempMech =

    type Route =
        | Route of Endpoint list * Target list

        static member empty =
            Route ([], [])

     and Endpoint =
        | Endpoint of int * UriTemplateRouteMethod * Pipeline

     and Target =
        | Template of Template
        | Literal of Literal

     and Template =
        | Template of Expression * Route // TODO

     and Literal =
        | Literal of Map<string,Route> * int
        | Empty


    let rec compile (routes: (int * UriTemplateRoute) list) =
        List.fold add Route.empty routes

    and add s =
        function | i, { Template = UriTemplate (parts)
                        Method = method
                        Pipeline = pipeline } -> s


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