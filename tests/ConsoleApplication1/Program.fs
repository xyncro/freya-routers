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

    static member endpoints_ =
        (fun (Route (es, _)) -> es), (fun es (Route (_, ts)) -> Route (es, ts))

    static member targets_ =
        (fun (Route (_, ts)) -> ts), (fun ts (Route (es, _)) -> Route (es, ts))

    static member empty =
            Route ([], [])

 and Endpoint =
    | Endpoint of int * UriTemplateRouteMethod * Pipeline

 and Target =
    | Template of Template
    | Literal of Literal

    static member template_ =
        (function | Template t -> Some t | _ -> None), (Template)

    static member literal_ =
        (function | Literal l -> Some l | _ -> None), (Literal)

    static member template =
        Template.Template >> Template

    static member literal =
        Literal.Literal >> Literal

 and Template =
    | Template of Expression * Route

 and Literal =
    | Literal of Map<string,Route> * int
    | Empty

    static member routes_ =
        (function | Literal (m, _) -> Some m | _ -> None),
        (fun m -> function | Literal (_, s) -> Literal (m, s) | _ -> Empty)

    static member size_ =
        (function | Literal (_, s) -> Some s | _ -> None),
        (fun s -> function | Literal (m, _) -> Literal (m, s) | _ -> Empty)

(* Optics

   Optics for accessing useful properties of the basic form of a route, a pair
   of an int and a UriTemplateRoute. These optics are used to simplify the
   interrogation and manipulation of state in the functions used to build and
   work with the Route instance. *)

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

(* Endpoints

   Functions to take an existing endpoint list and a list of int and
   UriTemplateRoute pairs (which have been pre-selected to be logical endpoints
   i.e. the UriTemplate defining the remaining route is empty) and return a
   list of endpoints with the appropriate endpoints added. *)

[<RequireQualifiedAccess>]
module Endpoints =

    let rec add es =
            map ()
        >>> append es

    and private map _ =
            List.map (create)

    and private append =
            List.append

    and private create (i, { Method = m; Pipeline = p }) =
            Endpoint (i, m, p)

(* Templates

   Functions to take a list of int and UriTemplateRoute pairs (which have been
   pre-selected to be Template (expression) based for the first part of the
   defining UriTemplate path) and produce a list of Targets, recurively using
   the route function to construct appropriate sub-routes.

   The routes are first grouped by the head expression value to simplify
   construction. *)

[<RequireQualifiedAccess>]
module Templates =

    let rec add route =
            group
        >>> map route

    and private group =
            List.groupBy (Optic.get expression_ >> Option.get)

    and private map route =
            List.map (create route)

    and private routes =
            List.map (Optic.map parts_ List.tail)

    and private create route (e, rs) =
            Target.template (e, route Route.empty (routes rs))

(* Literals *)

[<RequireQualifiedAccess>]
module Literals =

    let rec add route =
            sort
        >>> fold route
        >>> test

    and private sort =
            List.sortBy (Optic.get (literal_ >?> Literal.literal_) >> Option.get >> fun s -> s.Length)

    and private fold route =
            List.fold (upsert route) Route.empty

    and private upsert route s r =
            Optic.map Route.targets_ (fun ts ->
                (function | Some i -> update route i ts
                          | _ -> insert route ts) (tryFindIndex ts)) s

    and private update route i ts =
            ts

    and private insert route ts =
            ts

    and private tryFindIndex =
            List.tryFindIndex (Optic.get (Prism.ofEpimorphism Target.literal_) >> Option.isSome)

    and private test r =
            []

(* Targets *)

[<RequireQualifiedAccess>]
module Targets =

    let rec add route ts =
            partition
        >>> Templates.add route *** Literals.add route
        >>> join
        >>> append ts

    and private partition =
            List.partition (Optic.get expression_ >> Option.isSome)

    and private join (a, b) =
            List.append a b

    and private append =
            List.append

(* Routes *)

[<RequireQualifiedAccess>]
module Routes =

    let rec add route es ts =
            partition
        >>> Endpoints.add es *** Targets.add route ts

    and private partition =
            List.partition (Optic.get parts_ >> List.isEmpty)

(* Route *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Route =

    let rec add (Route (es, ts)) =
            Routes.add add es ts
        >>> Route

(* Main *)

[<EntryPoint>]
let main _ =

    let routes =
        [ 3, { Template = UriTemplate.parse "/users/{user}"
               Method = All
               Pipeline = Pipeline.next }
          1, { Template = UriTemplate.parse "/"
               Method = All
               Pipeline = Pipeline.next }
          2, { Template = UriTemplate.parse "/users"
               Method = All
               Pipeline = Pipeline.next } ]

    let route =
        Route.add Route.empty routes

    printfn "%A" route

    0
