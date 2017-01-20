open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let sq<[<Measure>] 'u>(x: float<'u>) = x * x

let G = 6.67408e-11<m^3/kg/s^2>

let earthMass = 5.97219e24<kg>

let moonMass = 7.34767309e22<kg> //80.0<kg>

let earthRadius = 6371000.0<m>
let moonRadius = 1737000.0<m>

let r = earthRadius + moonRadius

let Fem = G * earthMass * moonMass / sq r

let gEarth = G * earthMass / (sq earthRadius)
let gMoon = G * moonMass / (sq moonRadius)

let aEarth = Fem / earthMass

let d = 0.5 * aEarth * 1.0<s^2>