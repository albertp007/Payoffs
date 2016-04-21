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

open NUnit.Framework
open FsUnit
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open Payoffs.Option
open System.Threading
open System.Threading.Tasks
open System.Linq

module MC =

  type VarianceReduction =
  | None
  | ATV

  type Path<'a> =
    | Ordinary of seq<'a>
    | Antithetic of seq<'a> * seq<'a>

  type Paths<'a> =
    | Ordinarys of int * int * 'a[]
    | Antithetics of int * int * 'a[] * 'a[]

  let getPath paths i =
    match paths with
    | Ordinarys (rows, columns, a) -> 
        Ordinary (System.ArraySegment(a, i*columns, columns))
    | Antithetics (rows, columns, a, b) ->
        Antithetic (System.ArraySegment(a, i*columns, columns), 
          System.ArraySegment(b, i*columns, columns))

  let iterPaths f (paths: Paths<'a>) =
    let rows = match paths with
               | Ordinarys (r, _, _) -> r
               | Antithetics (r, _, _, _) -> r
    for i in 0..(rows-1) do
      getPath paths i |> f i

  let allocatePaths vr m n =
    match vr with
    | None -> (m, n, Array.zeroCreate (m*n)) |> Ordinarys
    | ATV ->  (m, n, Array.zeroCreate (m*n), Array.zeroCreate (m*n)) 
              |> Antithetics
 
  let mapInPlace f init startIndex length (arr: 'a[])  =
    for i in startIndex..(startIndex+length-1) do
      let prev = if i % length = 0 then init else arr.[i-1]
      arr.[i] <- f i prev arr.[i]

  let gbmGenPaths s0 r q v t n vr m =
      // allocate all memory required up front to minimzie thread contention
    // this should be super quick ideally
    let paths = allocatePaths vr m n
    let dist = Normal.WithMeanVariance(0.0, (t/(float n)))
    let flipSign (original: float[]) = fun i _ _ -> -original.[i]
    let calcPriceSample =
      let dt = t/float n
      let drift = (r - v**2.0/2.0) * dt
      fun _ prev current -> prev * exp ( drift + current * v)

    match paths with
    | Ordinarys (_, _, a) -> 
        dist.Samples(a)
        for i in 0..(m-1) do
          mapInPlace calcPriceSample s0 (i*n) n a
    | Antithetics (_, _, a, b) -> 
        dist.Samples(a)
        mapInPlace (flipSign a) 0.0 0 (b.Length) b
        for i in 0..(m-1) do
          mapInPlace calcPriceSample s0 (i*n) n a
          mapInPlace calcPriceSample s0 (i*n) n b   
    paths

  let calcPayoff (payoff: seq<'a>->float) path =
    match path with
    | Ordinary path -> payoff path
    | Antithetic (path, antiPath) -> 0.5 * (payoff path + payoff antiPath)

  let discountedPayoff (r: float) t payoff = payoff >> ((*) (exp (-r*t)))
   
  let mc m genPaths payoff =
    let payoffs: float[] = Array.zeroCreate m
    let paths: Paths<float> = genPaths m
    let payoffFunc = calcPayoff payoff
    let gatherPayoffs i path = payoffs.[i] <- payoffFunc path
    iterPaths gatherPayoffs paths
    let estimate = payoffs |> Array.average
    let var = payoffs.Variance()
    let stderr = sqrt (var/float payoffs.Length)
    (estimate, var, stderr)

  let mcpar numBatches numPaths genPaths payoff =
    let task = async { return mc numPaths genPaths payoff }
    let results = task 
                  |> List.replicate numBatches 
                  |> Async.Parallel
                  |> Async.RunSynchronously
    results 

  let european optionType strike (path:seq<float>) =
    let intrinsic = path.Last() - strike
    match optionType with
    | Call -> max 0.0 intrinsic
    | Put -> max 0.0 -intrinsic

  let barrier barrierUpDown barrierInOut optionType strike barrierLevel 
    (path:seq<float>) =

    let euroPayoff = european optionType strike path

    let triggered = 
      match barrierUpDown with
      | Up -> Seq.exists (fun x -> x > barrierLevel) path
      | Down -> Seq.exists (fun x -> x > barrierLevel) path

    match barrierInOut with 
    | In -> if triggered then euroPayoff else 0.0
    | Out -> if triggered then 0.0 else euroPayoff

  let barriermc s0 r q v t n vr updown inout optType strike barrierLevel m =
    
    let payoffFunc = 
      barrier updown inout optType strike barrierLevel
      |> discountedPayoff r t

    let genPath = gbmGenPaths s0 r q v t n vr
    mc m genPath payoffFunc