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
  open MathNet.Numerics.Statistics

  [<TestCase(0.0, 1.0, 1000000)>]
  let ``Draw samples from normal distribution``(mu, sigma, size) =
    let draw = drawNormal mu sigma size
    let samples = draw()
    samples.Variance() |> should (equalWithin 0.1) 1.0
    samples.Mean() |> should (equalWithin 0.1) 0.0

  [<TestCase(0.02, 0.4, 100.0, 0.25, 1, 5000000)>]
  let ``MC european call option with antithetic variance reduction``(r, sigma, s0, t, n, m) =
    let (estimate, var, std) =
      gbm r sigma s0 t n ATV
      |> genPaths m 
      |> mc (MC.european Call s0 |> discountedPayoff r t)
    estimate |> should (equalWithin (2.0*std)) 8.19755

  [<TestCase(0.02, 0.4, 100.0, 0.25, 1, 5000000)>]
  let ``MC european call option without variance reduction``(r, sigma, s0, t, n, m) =
    let (estimate, var, std) =
      gbm r sigma s0 t n None
      |> genPaths m 
      |> mc (MC.european Call s0 |> discountedPayoff r t)
    estimate |> should (equalWithin (2.0*std)) 8.19755

  [<TestCase(50.0, 0.05, 0.0, 0.4, 0.25, 5000)>]
  let ``Binomial - number of nodes``(s0, r, q, v, t, n) =
    let expected = (n+1)*(n+2)/2
    let tree = Binomial(s0, r, q, v, t, n)
    tree.NumNodes |> should equal expected    

  [<TestCase(50.0, 0.05, 0.0, 0.4, 0.25, 3)>]
  let ``Binomial calc up down``(s0, r, q, v, t, n) =
    let dt = t / float n
    let expected = exp ( v * sqrt dt) 
    let tree = Binomial(s0, r, q, v, t, n)
    let (u, _, _ ) = tree.GetProbabilities()
    u |> should equal expected

  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, 1, 52.0, 0, 0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 3, 1, 48.0, 2, 0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 4, 0, 52.0, 3, 3)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 2, 0, 48.0, 2, 0)>]
  let ``Binomial GetAssetPrice``(s0, r, q, v, t, n, optType, k, i, j) =
    let tree = Binomial(s0, r, q, v, t, n)
    let (u, _, _) = tree.GetProbabilities()
    let d = 1.0/u
    let factor = if j > 0 then u else d
    let expected = s0 * (factor ** float (abs j))
    tree.BuildGrid()
    tree.GetAssetPrice (i, j) |> should (equalWithin 0.01) expected

  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, 1, 52.0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, 1, 48.0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, 0, 52.0)>]
  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5, 0, 48.0)>]
  let ``Binomial GetIntrinsic``(s0, r, q, v, t, n, optType, k) =
    let optType' = if optType = 0 then Call else Put
    let expected = match optType' with
                   | Call -> s0 - k
                   | Put -> k - s0   
    let tree = Binomial(s0, r, q, v, t, n)
    tree.BuildGrid()
    tree.GetIntrinsic optType' k (0, 0) |> should (equalWithin 0.01) expected

  [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, 5000, 0, 100.0)>]
  let ``Binomial class european option``(s0, r, q, v, t, n, optType, k) =
    let optType' = if optType = 0 then Call else Put    
    let tree = Binomial(s0, r, q, v, t, n)
    let price = tree.Price (vanillaPayoff optType' k) europeanValue
    price |> should (equalWithin 0.05) 8.19775

  [<TestCase(50.0, 0.05, 0.0, 0.3, 2, 5000, 1, 52.0)>]
  let ``Binomial class american option``(s0, r, q, v, t, n, optType, k) =
    let optType' = if optType = 0 then Call else Put    
    let tree = Binomial(s0, r, q, v, t, n)
    let price = tree.Price (vanillaPayoff optType' k) 
                  (americanValue optType' k)
    price |> should (equalWithin 0.05) 7.47

  [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 3)>]
  let ``Binomial forward shooting grid states``(s0, r, q, v, t, n) =
    let tree = Binomial(s0, r, q, v, t, n)
    tree.BuildFSG americanLookbackCallState
    tree.StateValues |> should haveCount 13
    tree.StateValues.Keys |> should contain (0,0,0)
    tree.StateValues.Keys |> should contain (1,1,1)
    tree.StateValues.Keys |> should contain (1,-1,0)
    tree.StateValues.Keys |> should contain (2,0,0)
    tree.StateValues.Keys |> should contain (2,0,1)
    tree.StateValues.Keys |> should contain (2,-2,0)
    tree.StateValues.Keys |> should contain (2,2,2)
    tree.StateValues.Keys |> should contain (3,-3,0)
    tree.StateValues.Keys |> should contain (3,3,3)
    tree.StateValues.Keys |> should contain (3,-1,0)
    tree.StateValues.Keys |> should contain (3,-1,1)
    tree.StateValues.Keys |> should contain (3,1,2)
    tree.StateValues.Keys |> should contain (3,1,1)

    

              



