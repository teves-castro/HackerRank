module Test

open Expecto

[<EntryPoint>]
let main argv =
    // let config = {defaultConfig with verbosity = Logging.Warn}
    Tests.runTestsInAssembly defaultConfig argv
