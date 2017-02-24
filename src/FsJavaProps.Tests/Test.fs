module FsJavaProps

open Expecto

[<Tests>]
let tests =
    testCase "Sample" <| fun () ->
        Expect.equal 1 1 "eq"

[<EntryPoint>]
let main args = runTestsInAssembly defaultConfig args
