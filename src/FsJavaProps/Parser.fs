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
                            | 'f' -> parseChar (i+2) (builder.Append('\u000c'))
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
            let (endOfKey, keyContinuation, key) = parseStringUntil 0 (fun c -> c = ':' || c = '=' || c = ' ') line
            if keyContinuation then
                failwith "Multi-line not supported in keys"
            else if endOfKey = line.Length - 1 then
                Some (KeyValue(key, ""))
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

let parseOLD (text : string) =
    let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    fromLines lines

type CharReader = unit -> char option

let isWhitespace (c: char) = c = ' ' || c = '\t' || c = '\u00ff'

type IsEof =
    |Yes = 1y
    |No = 0y

let rec readToFirstChar (c: char option) (reader: CharReader) =
    match c with
    | Some ' '
    | Some '\t'
    | Some '\u00ff' ->
        readToFirstChar (reader ()) reader
    | Some '\r'
    | Some '\n' ->
        None, IsEof.No
    | Some _ -> c, IsEof.No
    | None -> None, IsEof.Yes

let readKey (c: char option) (reader: CharReader) (buffer: StringBuilder) =
    let rec recurseEnd (result: string) =
        match reader () with
        | Some ':'
        | Some '='
        | Some ' ' -> recurseEnd result
        | Some '\r'
        | Some '\n' -> result, false, None, IsEof.No
        | None -> result, false, None, IsEof.Yes
        | Some c -> result, true, Some c, IsEof.No
    let rec recurse (c: char option) (buffer: StringBuilder) (escaping: bool) =
        match c with
        | Some ' ' -> recurseEnd (buffer.ToString())
        | Some ':'
        | Some '=' when not escaping -> recurseEnd (buffer.ToString())
        | Some '\r'
        | Some '\n' -> buffer.ToString(), false, None, IsEof.No
        | None -> buffer.ToString(), false, None, IsEof.Yes
        | Some '\\' -> recurse (reader ()) (buffer.Append(c)) true
        | Some c -> recurse (reader ()) (buffer.Append(c)) false

    recurse c buffer false

let rec readComment (reader: CharReader) (buffer: StringBuilder) =
    match reader () with
    | Some '\r'
    | Some '\n' ->
        buffer.ToString(), IsEof.No
    | None ->
        buffer.ToString(), IsEof.Yes
    | Some c ->
        readComment reader (buffer.Append(c))

let rec readValue (c: char option) (reader: CharReader) (buffer: StringBuilder) =
    match c with
    | Some '\r'
    | Some '\n' ->
        buffer.ToString(), IsEof.No
    | None ->
        buffer.ToString(), IsEof.Yes
    | Some c ->
        readValue (reader()) reader (buffer.Append(c))

let rec readLine (reader: CharReader) (buffer: StringBuilder) =
    match readToFirstChar (reader ()) reader with
    | Some '#', _
    | Some '!', _ ->
        let comment, isEof = readComment reader (buffer.Clear())
        Some (Comment comment), isEof
    | Some firstChar, _ ->
        let key, hasValue, c, isEof = readKey (Some firstChar) reader (buffer.Clear())
        let value, isEof =
            if hasValue then
                // We know that we aren't at the end of the buffer, but readKey can return None if it didn't need the next char
                let firstChar = match c with | Some c -> Some c | None -> reader ()
                readValue firstChar reader (buffer.Clear())
            else
                "", isEof
        Some (KeyValue(key, value)), isEof
    | None, isEof -> None, isEof

let parse (text : string) =
    let len = text.Length
    let mutable i = 0
    let reader () =
        if i = len then
            None
        else
            let current = i
            i <- i + 1
            Some (text.[current])

    [
        let buffer = StringBuilder(255)
        let mutable isEof = IsEof.No
        while isEof <> IsEof.Yes do
            let line, isEofAfterLine = readLine reader buffer
            match line with
            | Some line -> yield line
            | None -> ()
            isEof <- isEofAfterLine
    ]
