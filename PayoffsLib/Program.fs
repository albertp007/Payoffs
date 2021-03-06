﻿//
// Payoffs - F# derivatives pricing library
// Copyright (c) 2016 by Albert Pang <albert.pang@me.com> 
// All rights reserved.
//
// This file is a part of Payoffs
//
// Payoffs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Payoffs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
module Main

open Payoffs.Option
open Payoffs.Lattice
open Payoffs.Product.Lattice
open Payoffs.MC
open System.Threading.Tasks

let testBinomial() =
  let tree = Binomial(100.0, 0.02, 0.0, 0.4, 0.25, 2000)
  let price = 
    tree.Price vanillaStateFunc (vanillaPayoff Call 100.0) europeanValue
  printfn "%A" price

let task1() =
  let g() = gbmGenPaths 100.0 0.02 0.0 0.4 0.25 3 ATV
  let payoff() = barrier Up Out Call 100.0 125.0 |> discountedPayoff 0.02 0.25
  mcpar 10 10000000 g payoff

let task2() =
  let g() = gbmGenPaths 100.0 0.02 0.0 0.4 0.25 3 ATV
  let payoff() = barrier Up Out Call 100.0 125.0 |> discountedPayoff 0.02 0.25  
  Parallel.For(0, 10, fun _ -> mc 1000000 (g()) (payoff()) |> ignore )

// [<EntryPoint>]
let main argv = 
  task2() |> ignore
  0 // return an integer exit code
