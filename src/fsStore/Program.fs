open System

open MBrace.FsPickler
open MBrace.FsPickler.Json
open MBrace.FsPickler.Combinators
open FsStore
open Test

module V2 =
    type Test =
        {
            b : float
            c : string
        } 


    let pTest_ (version : Version) =
        let inline pA () = Pickler.field (fun t -> 0) Pickler.int
        let inline pB () = Pickler.field (fun t -> t.b) Pickler.float
        let inline pC () = Pickler.field (fun t -> t.c) Pickler.string
        
        match version with
            | 0 -> 
                Pickler.product (fun a b -> { b = b; c = "was hot here" })
                ^+ pA() ^. pB()
            | 1 -> 
                Pickler.product (fun a b c -> { b = b; c = c })
                ^+ pA() ^+ pB() ^. pC()
            | 2 -> 
                Pickler.product (fun b c -> { b = b; c = c })
                ^+ pB() ^. pC()
            | _ -> failwith "version not recognized"

open V2

let currentVersion = 2

let pTest = versioned currentVersion V2.pTest_


(*

Features:
 * add field
 * remove field
 * patch old code to use new files
 * upgrade old files with old (patched) code

*)

[<EntryPoint;STAThread>]
let main argv = 
//    let o = { a = 1; b = 2.0 }
//    let s = Json.pickle pTest o
//    printfn "%s" s
    let v0 = """{"_version":0,"_vValue":{"Item1":1,"Item2":{"Item1":2.0,"Item2":null}}}"""
    let v0Parsed = Json.unpickle pTest v0
    printfn "%A" v0Parsed

    //let b = { a = 0; b = 4.5; c = "urdar" }
    //let sb = Json.pickle pTest b
    let v1 = """{"_version":1,"_vValue":{"Item1":0,"Item2":{"Item1":4.5,"Item2":{"Item1":"urdar","Item2":null}}}}"""
    let v1Parsed = Json.unpickle pTest v1
    printfn "%A" v1Parsed

    let c = { b = 1.5; c = "hello" }
    let v2 = Json.pickle pTest c

    let v2 = """{"_version":2,"_vValue":{"Item1":1.5,"Item2":{"Item1":"hello","Item2":null}}}"""
    let v2Parsed = Json.unpickle pTest v2

    let readV1DataFRomV2File = Json.unpickle (versioned 1 V0V1.pTest_) v2
    printfn "%A" readV1DataFRomV2File

    let changedOldData = { readV1DataFRomV2File with c = "does it work?" }
    let v1 = Json.pickle (versioned 2 V0V1.pTest_) changedOldData

    let v2WrittenWithOldPatchedCode = """{"_version":2,"_vValue":{"Item1":1.5,"Item2":{"Item1":"does it work?","Item2":null}}}"""
    let readUpgraded = Json.unpickle pTest v2WrittenWithOldPatchedCode

    printfn "%A" readUpgraded

    0
