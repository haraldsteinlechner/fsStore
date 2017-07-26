namespace Test

open MBrace.FsPickler
open MBrace.FsPickler.Json
open MBrace.FsPickler.Combinators
open FsStore

module V0V1 =

    type Test =
        {
            a : int
            b : float
            c : string
        } 

    let pTest_ (version : Version) =
        let inline pA () = Pickler.field (fun t -> t.a) Pickler.int
        let inline pB () = Pickler.field (fun t -> t.b) Pickler.float
        let inline pC () = Pickler.field (fun t -> t.c) Pickler.string
        match version with
            | 0 -> 
                Pickler.product (fun a b -> { a = a; b = b; c = "was hot here" })
                ^+ pA() ^. pB()
            | 1 -> 
                Pickler.product (fun a b c -> { a = a; b = b; c = c })
                ^+ pA() ^+ pB() ^. pC()
            | 2 -> 
                // upgrade case. patch code for reading new files into old code
                Pickler.product (fun b c -> { a = 0; b = b; c = c })
                ^+ pB() ^. pC()
            | _ -> failwith "version not recognized"