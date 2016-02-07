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
  let ``Pricing european call option with antithetic variance reduction``(r, sigma, s0, t, n, m) =
    let (estimate, var, std) =
      gbm r sigma s0 t n ATV
      |> genPaths m 
      |> mc (MC.european Call s0 |> discountedPayoff r t)
    estimate |> should (equalWithin (2.0*std)) 8.19755

  [<TestCase(0.02, 0.4, 100.0, 0.25, 1, 5000000)>]
  let ``Pricing european call option without variance reduction``(r, sigma, s0, t, n, m) =
    let (estimate, var, std) =
      gbm r sigma s0 t n None
      |> genPaths m 
      |> mc (MC.european Call s0 |> discountedPayoff r t)
    estimate |> should (equalWithin (2.0*std)) 8.19755

  [<TestCase(0.02, 0.4, 100.0, 0.25, 5000)>]
  let ``Binomial pricing european option``(r, v, s0, t, n) =
    let value = Lattice.price s0 r 0.0 v t n <| Lattice.european Call s0
    value |> should (equalWithin 0.05) 8.19755



