module FsJavaProps

open Expecto
open BlackFox.FsJavaProps

let private parse s = parseString s |> List.ofSeq

[<Tests>]
let fromDoc =
    testList "From doc" [
        testCase "Fruits" <| fun () ->
            let file = @"
fruits                           apple, banana, pear, \
                                    cantaloupe, watermelon, \
                                    kiwi, mango"
            let parsed = parse file
            let expected =
                [
                    KeyValue("fruits", "apple, banana, pear, cantaloupe, watermelon, kiwi, mango")
                ]

            Expect.equal parsed expected "eq"

        testCase "Truth" <| fun () ->
            let file = @"
Truth = Beauty
       Truth:Beauty
Truth                  :Beauty"
            let parsed = parse file
            let expected =
                [
                    KeyValue("Truth", "Beauty")
                    KeyValue("Truth", "Beauty")
                    KeyValue("Truth", "Beauty")
                ]

            Expect.equal parsed expected "eq"

        testCase "Cheeses" <| fun () ->
            let file = @"
cheeses"
            let parsed = parse file
            let expected =
                [
                    KeyValue("cheeses", "")
                ]

            Expect.equal parsed expected "eq"

        testCase "all escaped" <| fun () ->
            let file = @"
\:\= \:\=\r\nfo\o\u0020bar\t\f"
            let parsed = parse file
            let expected =
                [
                    KeyValue(":=", ":=\r\nfoo bar\t\u000c")
                ]

            Expect.equal parsed expected "eq"

        testCase "comments" <| fun () ->
            let file = @"
#Hello
   !World\
key=value"
            let parsed = parse file
            let expected =
                [
                    Comment("Hello")
                    Comment("World\\")
                    KeyValue("key", "value")
                ]

            Expect.equal parsed expected "eq"
        ]

[<Tests>]
let specialCases =
    testList "Special cases" [
        testCase "Space separator" <| fun () ->
            let parsed = parse "foo bar"
            let expected = [ KeyValue("foo", "bar") ]

            Expect.equal parsed expected "eq"

        testCase "Equal separator" <| fun () ->
            let parsed = parse "foo=bar"
            let expected = [ KeyValue("foo", "bar") ]

            Expect.equal parsed expected "eq"

        testCase "Two points separator" <| fun () ->
            Expect.equal
                (parse "foo:bar")
                ([ KeyValue("foo", "bar") ])
                "eq"

        testCase "Ends with escape" <| fun () ->
            Expect.equal
                (parse "foo:bar\\\r\n")
                ([ KeyValue("foo", "bar") ])
                "eq"

        testCase "Escaped tab in key" <| fun () ->
            Expect.equal
                (parse "fo\\to:bar")
                ([ KeyValue("fo\to", "bar") ])
                "eq"

        testCase "Escaped tab in value" <| fun () ->
            Expect.equal
                (parse "foo:ba\\tr")
                ([ KeyValue("foo", "ba\tr") ])
                "eq"

        testCase "Unicode in key" <| fun () ->
            Expect.equal
                (parse "\\uD83D\\udc0e")
                ([ KeyValue("\uD83D\udc0e", "") ])
                "eq"

        testCase "Unicode in value" <| fun () ->
            Expect.equal
                (parse "foo:\\uD83D\\udc0e")
                ([ KeyValue("foo", "\uD83D\udc0e") ])
                "eq"
    ]

[<Tests>]
let fullFiles =
    testList "Fullfiles" [
        testCase "1" <| fun () ->
            let parsed = parse @"#Fri Jan 17 22:37:45 MYT 2014
dbpassword=password
database=localhost
dbuser=vbfox"
            let expected =
                [
                    Comment("Fri Jan 17 22:37:45 MYT 2014")
                    KeyValue("dbpassword", "password")
                    KeyValue("database", "localhost")
                    KeyValue("dbuser", "vbfox")
                ]

            Expect.equal parsed expected "eq"
    ]

[<EntryPoint>]
let main args =
    runTestsInAssembly defaultConfig args
