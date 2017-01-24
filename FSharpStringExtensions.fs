namespace FSharpStringExtensions

// TODO: debate autoopen
//[<AutoOpen>]
module String =
    let fromByte (b:byte) =
        char b |> string

    let fromByteArray (b's:byte array) =
        let out =
            b's
            |> Array.Parallel.map char
        System.String(out)

    let ofArray (ss: string array) = ss |> Array.fold (+) ""

    let ofList (ss: string list) = ss |> List.fold (+) ""

    let ofSet (ss: Set<string>) = ss |> Set.fold (+) ""

    let ofSeq (ss: string seq) = ss |> Seq.fold (+) ""

    let rev (s: string) =
        let s' = s.ToCharArray() |> Array.rev
        new System.String(s')
    
    type TupleType =
        | String of (string * string)
        | Char of (char * char)
        | Byte of (byte * byte)

    let combineTuple (tTuple: TupleType) =
        match tTuple with
        | String (a, b) ->
            a + b
        | Char (a, b) ->
            (string a) + (string b)
        | Byte (a, b) ->
            (char a |> string) + (char b |> string)

//    let combineTuple' (tTuple: ('a * 'a)) =
//        match tTuple with
//        | (String a, String b) ->
//            a + b
//        | (Char a, Char b) ->
//            (string a) + (string b)
//        | (Byte a, Byte b) ->
//            (char a |> string) + (char b |> string)
//        |_ -> ""

    let interleave (s1: string) (s2: string) =
        let s1' = s1.ToCharArray()
        let s2' = s2.ToCharArray()
        Array.map2 (fun a b -> (string a) + (string b)) s1' s2'
        |> Array.fold (fun sta str -> sta + str) ""

//    let zip (a: string ) (b: string) =
//        let a' = a.ToCharArray()
//        let b' = b.ToCharArray()
//        Array.zip a' b'
//        |> Array.map TupleType
//        |> combineTuple