namespace Freya.Routers.UriTemplate

open System

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module Route =

        [<Obsolete ("Use data_ instead.")>]
        let Data_ =
            Route.data_

        [<Obsolete ("Use value_ instead.")>]
        let Value_ =
            Route.value_

        [<Obsolete ("Use atom_ instead.")>]
        let Atom_ =
            Route.atom_

        [<Obsolete ("Use list_ instead.")>]
        let List_ =
            Route.list_

        [<Obsolete ("Use keys_ instead.")>]
        let Keys_ =
            Route.keys_

    [<RequireQualifiedAccess>]
    module FreyaRouter =

        [<Obsolete ("Explicit conversion to a Freya Pipeline is no longer required in Freya.")>]
        let toPipeline =
            id