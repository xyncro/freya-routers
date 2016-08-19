#nowarn "46"

open Aether
open Aether.Operators
open Anat
open Anat.Operators
open Freya.Core
open Freya.Types.Http
open Freya.Types.Uri.Template

[<AutoOpen>]
module External =

    (* External Types *)

    type UriTemplateRoutes =
        | UriTemplateRoutes of UriTemplateRoute list

        static member routes_ =
            (fun (UriTemplateRoutes x) -> x), (UriTemplateRoutes)

     and UriTemplateRoute =
        { Method: UriTemplateRouteMethod
          Template: UriTemplate
          Pipeline: Pipeline }

        static member template_ =
            (fun x -> x.Template), (fun t x -> { x with Template = t })

     and UriTemplateRouteMethod =
        | All
        | Methods of Method list

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

(* Types

   Types defining a hybrid routing data structure, consisting of a simple tree
   of Templates (elements of a Uri Template, specifically Expression typed
   parts) and an adaptive prefix trie-like sructure for the definition of
   literal values.*)

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
    | Template of Expression * Route

 and Literal =
    | Literal of Map<string,Route> * int
    | Empty

(* Optics

   Optics for accessing useful properties of the basic form of a route, a pair
   of an int and a UriTemplateRoute. These optics are used to simplify the
   interrogation and manipulation of state in the functions used to build and
   work with the Route instance. *)

let private uriTemplate_ =
        snd_
    >-> UriTemplateRoute.template_

let private uriTemplateParts_ =
        uriTemplate_
    >-> UriTemplate.uriTemplate_

let private uriTemplateExpression_ =
        uriTemplateParts_
    >-> List.head_
    >?> UriTemplatePart.expression_

(* Endpoints

   Functions to take an existing endpoint list and a list of int and
   UriTemplateRoute pairs (which have been pre-selected to be logical endpoints
   i.e. the UriTemplate defining the remaining route is empty) and return a
   list of endpoints with the appropriate endpoints added. *)

let rec private endpoints es =
        endpointsMap ()
    >>> endpointsAppend es

and private endpointsMap _ =
        List.map (endpoint)

and private endpoint (i, { Method = m; Pipeline = p }) =
        Endpoint (i, m, p)

and private endpointsAppend =
        List.append

(* Templates

   Functions to take a list of int and UriTemplateRoute pairs (which have been
   pre-selected to be Template (expression) based for the first part of the
   defining UriTemplate path) and produce a list of Targets, recurively using
   the route function to construct appropriate sub-routes.

   The routes are first grouped by the head expression value to simplify
   construction. *)

let rec templates route =
        templatesGroup
    >>> templatesMap route

and private templatesGroup =
        List.groupBy (Optic.get uriTemplateExpression_ >> Option.get)

and private templatesMap route =
        List.map (templatesTarget route)

and private templatesTarget route (e, rs) =
        Target.Template (Template (e, route Route.empty (templatesRoutes rs)))

and private templatesRoutes =
        List.map (Optic.map uriTemplateParts_ List.tail)

(* Literals *)

let private literals _ =
        id

(* Targets *)

let rec targets route ts =
        targetsPartition
    >>> templates route *** literals route
    >>> targetsJoin
    >>> targetsAppend ts

and private targetsPartition =
        List.partition (Optic.get uriTemplateExpression_ >> Option.isSome)

and private targetsJoin =
        fst

and private targetsAppend =
        List.append

(* Routes *)

let rec routes route es ts =
        routesPartition
    >>> endpoints es *** targets route ts

and private routesPartition =
        List.partition (Optic.get uriTemplateParts_ >> List.isEmpty)

(* Route *)

let rec route (Route (es, ts)) =
        routes route es ts
    >>> Route

(*

Given a Route, and list of the routes (perhaps as remaining lists of parts, etc)
that form the potential set of endpoints and targets for this Route:
- Partition in to endpoints and targets - add endpoints to this Route (?)
- Partition targets in to templates and literals, where the first part determines the split
- For the set of templates
  - Group by the first part
  - For each group, repeat the overall process with a new emoty Route, and the remaining
    routes


*)

(* Main *)

[<EntryPoint>]
let main _ =

    let routes =
        [ 1, { Template = UriTemplate.parse "{one}"
               Method = All
               Pipeline = Pipeline.next }
          2, { Template = UriTemplate.parse "{one}{two}"
               Method = All
               Pipeline = Pipeline.next }
          3, { Template = UriTemplate.parse "{three}"
               Method = All
               Pipeline = Pipeline.next } ]

    let route =
        route Route.empty routes

    printfn "%A" route

    0
