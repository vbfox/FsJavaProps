module Parser

open System.Text
open System

type PropertiesFileEntry =
| Comment of text : string
| KeyValue of key : string * value : string

type PropertiesFile = PropertiesFileEntry list

let fromLines (lines : string seq) =
    let parseStringUntil (from : int) (f : char -> bool) (s : string) =
        let rec parseChar (i : int) (builder : StringBuilder) =
            if i >= s.Length then
                i-1, false, builder
            else
                let c = s.[i]
                if f c then
                    i-1, false, builder
                else
                    match c with
                    | '\\' ->
                        if i+1 >= s.Length then
                            i, true, builder
                        else
                            let nextChar = s.[i+1]
                            match nextChar with
                            | 'n' -> parseChar (i+2) (builder.Append('\n'))
                            | 'r' -> parseChar (i+2) (builder.Append('\r'))
                            | 't' -> parseChar (i+2) (builder.Append('\t'))
                            | 'u' -> failwith "Unicode escape not supported"
                            | nextChar -> parseChar (i+2) (builder.Append(nextChar))
                    | c -> parseChar (i+1) (builder.Append(c))

        let (i, continuation, builder) = parseChar from (new StringBuilder())
        i, continuation, builder.ToString()

    let rec continuationLineParser (lines : string list) =
        match lines with
        | [] -> failwith "Expected a continuation but reached the last line"
        | line :: rest ->
            let line = line.TrimStart()
            match parseStringUntil 0 (fun _ -> false) line with
            | _, true, text ->
                let (remainingLines, x) = continuationLineParser rest
                remainingLines, text + x
            | _, false, text -> rest, text

    let normalLineParser (line : string) (nextLines : string list) =
        let line = line.TrimStart()
        let firstChar = if line.Length > 0 then Some(line.[0]) else None
        match firstChar with
        | None -> None
        | Some(c) when c ='#' || c = '!' ->
            let comment = line.Substring(1)
            Some (Comment comment)
        | _ ->
            let (endOfKey, keyContinuation, key) = parseStringUntil 0 (fun c -> c = ':' || c = '=') line
            if keyContinuation then
                failwith "Multi-line not supported in keys"
            else if endOfKey = line.Length - 1 then
                failwithf "Key-value separator not found on line: %s" line
            else
                let key = key.TrimEnd()
                let (_, valueContinuation, value : string) = parseStringUntil (endOfKey+1) (fun _ -> false) line
                let value = value.TrimStart()
                (*let rest, value =
                    if valueContinuation then
                        value + (continuationLineParser nextLines) else value*)

                Some (KeyValue(key, value))

    let rec parser (remaining : string list) =
        match remaining with
        | [] -> []
        | line :: rest ->
            match normalLineParser line rest with
            | None -> parser rest
            | Some(content) -> content :: (parser rest)

    parser (Seq.toList lines)

let parse (text : string) =
    let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    fromLines lines
