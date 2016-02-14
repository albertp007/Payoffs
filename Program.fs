module Main

open Payoffs.Lattice
open Payoffs.Option

[<EntryPoint>]
let main argv = 
    let tree = Binomial(100.0, 0.02, 0.0, 0.4, 0.25, 2000)
    let price = tree.Price vanillaStateFunc (vanillaPayoff Call 100.0) europeanValue
    printfn "%A" price
    0 // return an integer exit code