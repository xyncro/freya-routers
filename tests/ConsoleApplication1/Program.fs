#nowarn "46"

open Aether
open Aether.Operators
open Anat
open Anat.Operators
open Freya.Core
open Freya.Routers
open Freya.Types.Http
open Freya.Types.Uri
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
    | Expression of Expression
    | Trie of Trie

    static member expression_ =
        (function | Expression t -> Some t | _ -> None), (Expression)

    static member trie_ =
        (function | Trie l -> Some l | _ -> None), (Trie)

 and Expression =
    | Expression of Template.Expression * Route

 and Trie =
    | Trie of Map<string,Route> * int

(* Optics

   Optics for accessing useful properties of the basic form of a route, a pair
   of an int and a UriTemplateRoute. These optics are used to simplify the
   interrogation and manipulation of state in the functions used to build and
   work with the Route instance. *)

(* int * UriTemplate *)

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

(* Target *)

let private trie_ =
        Prism.ofEpimorphism Target.trie_

//(* Common
//
//   Common functions for working with data structures used in the construction
//   of Route data. *)
//
//let private key =
//        Optic.get string_ >> Option.get
//
//let private tail =
//        Optic.map parts_ List.tail
//
//(* Endpoints
//
//   Functions to take an existing endpoint list and a list of int and
//   UriTemplateRoute pairs (which have been pre-selected to be logical endpoints
//   i.e. the UriTemplate defining the remaining route is empty) and return a
//   list of endpoints with the appropriate endpoints added. *)
//
//let rec private endpoints es =
//        List.map endpoint
//    >>> List.append es
//
//and private endpoint (i, { Method = m; Pipeline = p }) =
//        Endpoint (i, m, p)
//
//(* Expressions
//
//   Functions to take a list of int and UriTemplateRoute pairs (which have been
//   pre-selected to be expression based for the first part of the defining
//   UriTemplate path) and produce a list of Targets, recursively using the route
//   function to construct appropriate sub-routes.
//
//   The routes are first grouped by the head expression value to simplify
//   construction. *)
//
//let rec private expressions route =
//        List.groupBy (Optic.get expression_ >> Option.get)
//    >>> List.map (expression route)
//
//and private expression route (e, rs) =
//        Target.expression (e, route Route.empty (List.map (Optic.map parts_ List.tail) rs))
//
//(* Strings *)
//
//let rec private strings route =
//        List.sortBy (Optic.get string_ >> Option.get >> fun s -> s.Length)
//    >>> List.fold (fun s r -> upsert route r s) []
//
//and private upsert route r =
//        List.partition (Optic.get strings_ >> Option.isSome)
//    >>> choose route r *** id
//    >>> (fun (a, b) -> printfn "joining %A :: %A" a b; List.append a b)
//
//and private choose route r =
//        function | [] ->
//                        printfn "choose singleton %A" r
//                        List.singleton (create route r)
//                 | ts ->
//                        printfn "choose map %A - %A" r ts 
//                        List.map (update route r) ts
//
//
//
//
//
//
//
//
//
//and private update route r t =
//        Optic.map strings_ (
//            function | Strings (m, s) ->
//
//                            let k = (Optic.get string_ >> Option.get) r
//
//                            let m2 =
//                                (function | l when l = s ->
//                                                printfn "adding"
//                                                Optic.map (Map.value_ k) (
//                                                    function | Some r' -> Some (route r' [ Optic.map parts_ List.tail r ])
//                                                             | _ -> Some (route Route.empty [ Optic.map parts_ List.tail r ])) m
//                                          | _ ->
//                                                printfn "adding with diff length..."
//
//                                                let k2 = k.Substring (0, s)
//
//
//
//                                                Optic.map (Map.value_ k2) (
//                                                    function | Some (Route (es, ts)) ->
//                                                                    
//                                                                    let r2 = Optic.map string_ (fun k -> k.Substring (s)) r
//                                                                    
//                                                                    Some (Route (es, ts))
//                                                                    
//                                                                    //Some (route rx [ r2 ])
//                                                             | _ ->
//                                                                    Some (route Route.empty [ Optic.map string_ (fun k -> printfn "new %s" k; k.Substring (s)) r ])) m) k.Length
//                            
//                            Strings (m2, s)) t
//
//
//
//
//
//
//and private create route =
//        Optic.get string_ >> Option.get &&& Optic.map parts_ List.tail
//    >>> target route
//
//and private target route (k, v) =
//        Target.strings (Map.ofList [ k, route Route.empty [ v ] ], k.Length)
//
//(* Targets *)
//
//let rec private targets route ts =
//        List.partition (Optic.get expression_ >> Option.isSome)
//    >>> expressions route *** strings route
//    >>> List.join
//    >>> List.append ts

//let rec private routes route es ts =
//        List.partition (Optic.get parts_ >> List.isEmpty)
//    >>> endpoints es *** targets route ts

(* Route

   A function for adding a new logical route (defined by an int and
   UriTemplateRoute pair) to an existing Route, assuming that Route is the
   base of the routing tree structure. *)

//let rec private route (Route (es, ts)) =
//    function | i, { Method = m
//                    Template = UriTemplate []
//                    Pipeline = p } -> Route (endpoint i m p es, ts)
//             | i, { Method = m
//                    Template = UriTemplate (UriTemplatePart.Literal (Literal s) :: ps)
//                    Pipeline = p } -> Route (es, strings i m s ps p ts)
//             | _ -> Route (es, ts)
//
//and endpoint i m p es =
//    Endpoint (i, m, p) :: es
//
//and strings i m s ps p =
//        List.tryExtract strings_
//    >>> upsert i m s ps p *** id
//    >>> List.consPair
//        
//and upsert i m s ps p =
//    function | Some r -> Target.Strings r
//             | _ -> Target.Strings (Strings (Map.empty, 0))




(* Routes *)

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
        function | Trie (m, s) when k.Length = s -> Trie (Optic.map (Map.value_ k) (equal rs) m, s)
                 | Trie (m, s) -> Trie (Optic.map (Map.value_ (k.Substring (0, s))) (greater (k.Substring (s)) rs) m, s)

and private equal rs =
        function | Some r -> Some (routes r rs)
                 | _ -> Some (routes Route.empty rs)

and private greater k rs =
        function | Some (Route (es, ts)) -> Some (Route (es, upsert (k, rs) ts))
                 | _ -> Some (Route ([], upsert (k, rs) []))

and private insert (k, rs) =
        Trie (Map.ofList [ k, routes Route.empty rs ], k.Length)

(* Main *)

[<EntryPoint>]
let main _ =

    let data =
        [ 1, { Template = UriTemplate.parse "/"
               Method = All
               Pipeline = Pipeline.next }
          2, { Template = UriTemplate.parse "/users"
               Method = All
               Pipeline = Pipeline.next }
          3, { Template = UriTemplate.parse "/users/{user}"
               Method = All
               Pipeline = Pipeline.next }
          4, { Template = UriTemplate.parse "/groups"
               Method = All
               Pipeline = Pipeline.next }
          5, { Template = UriTemplate.parse "/groups/{group}"
               Method = All
               Pipeline = Pipeline.next }
          6, { Template = UriTemplate.parse "/groups/{group}/name"
               Method = All
               Pipeline = Pipeline.next } ]

    let route =
        routes Route.empty data

    printfn "%A" route

    0
