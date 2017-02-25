module FsJavaProps

open Expecto
open Parser

[<Tests>]
let tests =
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

        testCase "Space separator" <| fun () ->
            let parsed = parse "foo bar"
            let expected = [ KeyValue("foo", "bar") ]

            Expect.equal parsed expected "eq"

        testCase "Equal separator" <| fun () ->
            let parsed = parse "foo=bar"
            let expected = [ KeyValue("foo", "bar") ]

            Expect.equal parsed expected "eq"

        testCase "Two points separator" <| fun () ->
            let parsed = parse "foo:bar"
            let expected = [ KeyValue("foo", "bar") ]

            Expect.equal parsed expected "eq"
    ]

let xxx =
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

[<EntryPoint>]
let main args =
    //runTests defaultConfig xxx
    runTestsInAssembly defaultConfig args
