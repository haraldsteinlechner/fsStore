namespace FsStore

open System
open MBrace.FsPickler
open MBrace.FsPickler.Json
open MBrace.FsPickler.Combinators

type Version = int
module Pickler =
    let version = Pickler.int

[<AutoOpen>]
module Versioned =
    let versioned (currentVersion : Version) (p : int -> Pickler<'a>) : Pickler<'a> =
        let read (rs : ReadState) =
            let version = Pickler.version.Read rs "_version"
            let p = p version
            p.Read rs "_vValue"

        let write (ws : WriteState) =
            Pickler.version.Write ws "_version" currentVersion
            let p = p currentVersion
            p.Write ws "_vValue"

        Pickler.FromPrimitives(read,write)