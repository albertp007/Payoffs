//
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
namespace Payoffs

module UnitTests = 
  open NUnit.Framework
  open FsUnit
  open Payoffs.Option
  open Payoffs.MC
  open Payoffs.Lattice
  open Payoffs.Product.Lattice
  open MathNet.Numerics.Statistics
  open System.Collections.Generic
  
  let toOptType isCall = 
    match isCall with
    | true -> Call
    | false -> Put
  
  let toUpDown isUp = 
    match isUp with
    | true -> Up
    | false -> Down

  let toBarrierType isKI =
    match isKI with
    | true -> In
    | false -> Out
  
  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, 100.0)>]
  let ``Black Scholes equation put-call parity`` (s0, r, q, v, t, k) = 
    let call = blackScholes s0 r q v t Call k
    let put = blackScholes s0 r q v t Put k
    put + s0 |> should (equalWithin 0.0001) (call + k * exp (-r * t))
  
  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, 100.0, 8.19755391)>]
  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, false, 100.0, 7.69880183)>]
  let ``European Implied Volatility`` (s0, r, q, v, t, isCall, k, price) = 
    let precision = 0.0001
    let maxIter = 100
    let optType = toOptType isCall
    let price' = blackScholes s0 r q v t optType k
    let (v', _) = impliedVolatility precision maxIter s0 r q t k optType price
    v' |> should (equalWithin precision) v
  
  [<TestCase(0.0, 1.0, 1000000)>]
  let ``Draw samples from normal distribution`` (mu, sigma, size) = 
    let draw = drawNormal mu sigma size
    let samples = draw()
    samples.Variance() |> should (equalWithin 0.1) 1.0
    samples.Mean() |> should (equalWithin 0.1) 0.0
  
  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, 1, 5000000)>]
  let ``MC european call option with antithetic variance reduction`` (s0, r, q, 
                                                                      v, t, n, 
                                                                      m) = 
    let (estimate, var, std) = 
      gbm s0 r q v t n ATV
      |> genPaths m
      |> mc (MC.european Call s0 |> discountedPayoff r t)
    
    let q = 0.0
    let expected = blackScholes s0 r q v t Call s0
    estimate |> should (equalWithin (2.0 * std)) expected
  
  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, 1, 5000000)>]
  let ``MC european call option without variance reduction`` (s0, r, q, v, t, n, 
                                                              m) = 
    let (estimate, var, std) = 
      gbm s0 r q v t n None
      |> genPaths m
      |> mc (MC.european Call s0 |> discountedPayoff r t)
    
    let q = 0.0
    let expected = blackScholes s0 r q v t Call s0
    estimate |> should (equalWithin (2.0 * std)) expected
  
  [<TestCase(50.0, 0.05, 0.0, 0.4, 0.25, 5000)>]
  let ``Binomial - number of nodes`` (s0, r, q, v, t, n) = 
    let expected = (n + 1) * (n + 2) / 2
    let tree = Binomial(s0, r, q, v, t, n)
    tree.NumNodes |> should equal expected
  
  [<TestCase(50.0, 0.05, 0.0, 0.4, 0.25, 3)>]
  let ``Binomial calc up down`` (s0, r, q, v, t, n) = 
    let dt = t / float n
    let expected = exp (v * sqrt dt)
    let tree = Binomial(s0, r, q, v, t, n)
    tree.Up |> should equal expected
  
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, 0, 0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 3, 2, 0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 4, 3, 3)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 2, 2, 0)>]
  let ``Binomial GetAssetPrice`` (s0, r, q, v, t, n, i, j) = 
    let tree = Binomial(s0, r, q, v, t, n)
    let d = 1.0 / tree.Up
    
    let factor = 
      if j > 0 then tree.Up
      else d
    
    let expected = s0 * (factor ** float (abs j))
    tree.BuildGrid()
    tree.GetAssetPrice(i, j) |> should (equalWithin 0.01) expected
  
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, false, 52.0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, false, 48.0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, true, 52.0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, true, 48.0)>]
  let ``Binomial GetIntrinsic`` (s0, r, q, v, t, n, isCall, k) = 
    let optType' = toOptType isCall
    
    let expected = 
      match optType' with
      | Call -> s0 - k
      | Put -> k - s0
    
    let tree = Binomial(s0, r, q, v, t, n)
    tree.BuildGrid()
    tree.GetIntrinsic optType' k (0, 0) |> should (equalWithin 0.01) expected
  
  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, 2000, true, 100.0)>]
  let ``Binomial class european option`` (s0, r, q, v, t, n, isCall, k) = 
    let optType' = toOptType isCall
    let tree = Binomial(s0, r, q, v, t, n)
    let price = 
      tree.Price vanillaStateFunc (vanillaPayoff optType' k) europeanValue
    let expected = blackScholes s0 r q v t Call s0
    price |> should (equalWithin 0.01) expected
  
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 2000, false, 52.0)>]
  let ``Binomial class american option`` (s0, r, q, v, t, n, isCall, k) = 
    let optType' = toOptType isCall
    let tree = Binomial(s0, r, q, v, t, n)
    let price = 
      tree.Price vanillaStateFunc (vanillaPayoff optType' k) 
        (americanValue optType' k)
    price |> should (equalWithin 0.01) 7.47
  
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 3)>]
  let ``Binomial forward shooting grid states`` (s0, r, q, v, t, n) = 
    let tree = Binomial(s0, r, q, v, t, n)
    let containsKey (tables : Dictionary<int, float> []) (i, j, k) = 
      tables.[tree.ToIndex(i, j)].ContainsKey(k)
    let numItems (tables : Dictionary<int, float> []) = 
      Array.fold 
        (fun count (table : Dictionary<int, float>) -> count + table.Count) 0 
        tables
    tree.BuildFSG lookbackPutState
    numItems tree.StateValues |> should equal 13
    let correctStates = 
      [ (0, 0, 0)
        (1, 1, 1)
        (1, -1, 0)
        (2, 0, 0)
        (2, 0, 1)
        (2, -2, 0)
        (2, 2, 2)
        (3, -3, 0)
        (3, 3, 3)
        (3, -1, 0)
        (3, -1, 1)
        (3, 1, 2)
        (3, 1, 1) ]
    List.forall (fun state -> containsKey tree.StateValues state) correctStates 
    |> should equal true
  
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 3)>]
  let ``Binomial Lookback Put`` (s0, r, q, v, t, n) = 
    let tree = Binomial(s0, r, q, v, t, n)
    let price = 
      tree.Price lookbackPutState lookbackPutPayoff (lookbackPutValue American)
    price |> should (equalWithin 0.01) 5.47
  
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 1000, true, false, true, 50.0, 60.0, 0.5844)>]
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 1000, true, false, false, 50.0, 60.0, 3.2292)>]
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 1000, false, false, true, 50.0, 40.0, 4.5392)>]  
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 1000, false, false, false, 50.0, 40.0, 0.9471)>]
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 1000, true, true, true, 50.0, 60.0, 3.995996772)>]
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 1000, true, true, false, 50.0, 60.0, 0.1167678526)>]
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 1000, false, true, true, 50.0, 40.0, 0.04122001074)>]  
  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 1000, false, true, false, 50.0, 40.0, 2.398845761)>]
  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, 3000, true, false, true, 100.0, 125.0, 2.0778816131077993)>]
  let ``Binomial Barrier`` (s0, r, q, v, t, n, isUp, isKI, isCall, k, ko, 
                             expected) = 
    let tree = Binomial(s0, r, q, v, t, n)
    let price = 
      tree.Price (barrierState (toUpDown isUp) tree ko) 
        (barrierPayoff (toBarrierType isKI) (toOptType isCall) k) 
        (barrierValue tree)
    price |> should (equalWithin 0.01) expected

  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 3.3)>]
  let ``MC barrier option with antithetic variance reduction`` 
    ( s0, r, q, v, t, isUp, isIn, isCall, b, k, n, m, expected ) = 

    let upDown = toUpDown isUp
    let inOut = toBarrierType isIn
    let optType = toOptType isCall
    let payoff = MC.barrier upDown inOut optType k b
    let (estimate, var, std) = 
      gbm s0 r q v t n ATV
      |> genPaths m
      |> mc (payoff |> discountedPayoff r t)
    
    estimate |> should (equalWithin 0.01) expected