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

open MathNet.Numerics.Distributions

module Option =

  type OptionType =
  | Call
  | Put

  type ExerciseType =
  | European
  | American

  type BarrierDirection =
  | Up
  | Down

  type BarrierType =
  | In
  | Out

  /// <summary>
  /// Black-Scholes formula
  /// </summary>
  /// <param name="s0">Underlying price</param>
  /// <param name="r">Interest rate</param>
  /// <param name="q">Convenience yield</param>
  /// <param name="v">Volatility</param>
  /// <param name="t">Time to expiry in years</param>
  /// <param name="optType">Call/Put</param>
  /// <param name="k">Strike</param>
  /// <returns>Black-Scholes price of the vanilla option</returns>
  let blackScholes s0 r q v t optType k =
    let f = s0 * exp( (r-q) * t )
    let d1 = 1.0 / v / (sqrt t) * (log (s0/k) + (r-q+0.5*v*v)*t)
    let d2 = d1 - v*(sqrt t)
    let n x = Normal.CDF(0.0, 1.0, x)
    match optType with
    | Call -> exp(-r*t)*(f*n(d1)-k*n(d2))
    | Put -> exp(-r*t)*(k*n(-d2)-f*n(-d1))

  /// <summary>
  /// Calculate implied volatility of a vanilla european option using
  /// Newton-Raphson method.  The initial guess is based on Brenner, Menachem 
  /// and Marti G. Subrahmanyam, 1988. A Simple Formula to Compute Implied 
  /// Standard Deviation, Financial Analyst Journal 5, 80-83
  /// </summary>
  /// <param name="precision">Iteration stops once the difference between
  /// iterations is smaller than this number</param>
  /// <param name="maxIteration">Iteration stops when the number of iteration
  /// exceeds the specified number</param>
  /// <param name="s0">Underlying price</param>
  /// <param name="r">Interest rate</param>
  /// <param name="q">Convenience yield holding the underlying</param>
  /// <param name="t">Time to expiry</param>
  /// <param name="k">Strike</param>
  /// <param name="optType">Call/Put</param>
  /// <param name="p">Vanilla european option price</param>
  let impliedVolatility precision maxIteration s0 r q t k optType p =
    // Newton-Raphson
    let bs v = blackScholes s0 r q v t optType k
    // initial guess
    let v0 = sqrt( 2.0*System.Math.PI/t) * p / s0
    let vega v = 
      let d1 = 1.0 / v / (sqrt t) * (log (s0/k) + (r-q+0.5*v*v)*t)
      s0 * Normal.PDF(0.0, 1.0, d1) * sqrt t
    let iv v = v - (bs(v)-p)/vega v
    let rec iter (precision:float) maxIteration n f init =
      let current = f init
      if current - init < precision || n >= maxIteration then (current, n)
      else iter precision maxIteration (n+1) f current
    iter precision maxIteration 0 iv v0

