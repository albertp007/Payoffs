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

  /// <summary>
  /// Type for indicating scheme for variance reduction
  /// <para>None - No variance reduction</para>
  /// <para>ATV - Antithetic variates</para>
  /// </summary>
  type VarianceReduction =
  | None
  | ATV

  /// <summary>
  /// Represents a single path as a sequence
  /// <para>Ordinary - sequence of values, e.g. floats</para>
  /// <para>Antithetic - a pair of sequence of values, which is antithetic of
  /// each other</para>
  /// </summary>
  type Path<'a> =
    | Ordinary of seq<'a>
    | Antithetic of seq<'a> * seq<'a>

  /// <summary>
  /// Represents an array of paths by a flat array.  This is to make memory
  /// allocation as efficient as possible for large number of paths.  Use the
  /// <c>getPath</c> function to extract individual path
  /// <para>Ordinarys - a triplet for number of rows, columns and the array of
  /// values</para>
  /// <para>Antithetics - a quadruple for number of rows, columns and two arrays
  /// of values which are antithetic of each other</para>
  /// </summary>
  type Paths<'a> =
    | Ordinarys of int * int * 'a[]
    | Antithetics of int * int * 'a[] * 'a[]

  /// <summary>
  /// Gets the number of rows and columns in an instance of Paths
  /// </summary>
  /// <param name="paths">The instance of Paths</param>
  let getNumOfPaths paths =
    match paths with
    | Ordinarys (rows, columns, _) -> (rows, columns)
    | Antithetics (rows, columns, _, _) -> (rows, columns)

  /// <summary>
  /// Gets an individual Path instance from a Paths instance at the given index
  /// </summary>
  /// <param name="paths">the Paths</param>
  /// <param name="i">the index of the Path to get</param>
  /// <returns>A Path instance</returns>
  let getPath paths i =
    let (r, _) = getNumOfPaths paths
    if i >= r then failwith (sprintf "The Paths instance only has %d rows" r)
    match paths with
    | Ordinarys (rows, columns, a) -> 
        Ordinary (System.ArraySegment(a, i*columns, columns))
    | Antithetics (rows, columns, a, b) ->
        Antithetic (System.ArraySegment(a, i*columns, columns), 
          System.ArraySegment(b, i*columns, columns))

  /// <summary>
  /// Iterate through the paths in the given Paths instance and apply function f
  /// on each Path
  /// </summary>
  /// <param name="f">Function to be applied to each path. The index i of the
  /// Path is passed as the first argument and the Path instance itself is
  /// passed in the second argument</param>
  /// <param name="paths">The Paths instance to iterate</param>
  let iterPaths f (paths: Paths<'a>) =
    let rows = match paths with
               | Ordinarys (r, _, _) -> r
               | Antithetics (r, _, _, _) -> r
    for i in 0..(rows-1) do
      getPath paths i |> f i

  /// <summary>
  /// Allocates memory for a Paths instance with the given number of rows and
  /// columns and variance reduction scheme.  The points in all the paths are
  /// stored in one flat array of size m * n (where m is the number of paths
  /// and n is the number of points in each path) if there is no variance
  /// reduction.  If antithetic variates is used, then two arrays, each of size
  /// m * n are allocated.  This makes memory allocation very fast without the 
  /// need to allocate e.g. array of arrays which are slower.  Access into
  /// individual paths are via the getPath and iterPaths functions.  e.g. if
  /// there are 10 paths, and each path has 3 data points, an array of 30
  /// elements is allocated.  The 0 index path is element 0, 1, 2.  The 1 index
  /// path is element 3, 4, 5.  In general, the n index path is element n, n+1,
  /// ..., n+length-1. This also allows memory allocation to be done and over
  /// with in one go, minimizing the chance of contentions for the memory
  /// allocator when run in parallel with multiple threads, in exchange for the
  /// slight inconvenience in the accessing of the individual path instances,
  /// the performance gain is substantial.  The arrays allocated are initialized
  /// to zero.
  /// </summary>
  /// <param name="vr">The variance reduction scheme to be used</param>
  /// <param name="m">The number of paths</param>
  /// <param name="n">The number of points in each path</param>
  let allocatePaths vr m n =
    match vr with
    | None -> (m, n, Array.zeroCreate (m*n)) |> Ordinarys
    | ATV ->  (m, n, Array.zeroCreate (m*n), Array.zeroCreate (m*n)) 
              |> Antithetics

  /// <summary>
  /// Maps each element in the raw array in the Paths, passes the index of the
  /// current element, the previous element and the current element to a
  /// function f whose return value is then used to update the current element.
  /// This is a convenience function to deal with modifying in-place the 
  /// elements in the raw array, representing a Path within a Paths instance.  
  /// Modifying the array in place is somewhat not very functional.  However, 
  /// this avoids incurring any memory allocation during rocessing and reduces
  /// contention for memory allocation when run with multiple threads.
  /// </summary>
  /// <param name="f">The function to apply to each element</param>
  /// <param name="init">The initial value to use as the prev value needed by
  /// f when the current element is the first element in the path</param>
  /// <param name="startIndex">The index to start the mapping process</param>
  /// <param name="length">The length of elements to map</param>
  /// <param name="arr">The raw array</param>
  let mapInPlace f init startIndex length (arr: 'a[])  =
    for i in startIndex..(startIndex+length-1) do
      // length is supposed to mean the number of points in each Path.
      // Therefore when the index is a multiple of length, the elements from i
      // to i + length (exclusive) are those of a particular Path within the
      // array and the element at i is the first point in that Path.  Therefore
      // instead of using the element previous to it in the raw array, we use
      // the supplied init value which typically is meant to be the initial
      // price
      let prev = if i % length = 0 then init else arr.[i-1]
      arr.[i] <- f i prev arr.[i]

  /// <summary>
  /// Generates a Paths instance based on geometric brownian motion.
  /// </summary>
  /// <param name="s0">Initial stock price</param>
  /// <param name="r">Risk-free rate</param>
  /// <param name="q">Convenience yield</param>
  /// <param name="v">Volatility p.a.</param>
  /// <param name="t">Time to expiry in years</param>
  /// <param name="n">Number of points to generate in path</param>
  /// <param name="vr">Variance reduction scheme</param>
  /// <param name="m">The number of paths to generate</param>
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
        dist.Samples(a) // populates with normal samples
        for i in 0..(m-1) do // for each path, calculate sample prices in path
          mapInPlace calcPriceSample s0 (i*n) n a
    | Antithetics (_, _, a, b) -> 
        dist.Samples(a) // populates with normal samples
        mapInPlace (flipSign a) 0.0 0 (b.Length) b // populates the antithetics
        for i in 0..(m-1) do
          mapInPlace calcPriceSample s0 (i*n) n a
          mapInPlace calcPriceSample s0 (i*n) n b   
    paths

  /// <summary>
  /// Calculates the payoff of a Path instance by first extracting the sample
  /// prices as a sequence of prices and pass the sequence to the payoff
  /// function supplied.  If antithetic variates is used, the resulting payoff
  /// is the average of the payoff of the original path and its antithetic
  /// variate
  /// </summary>
  /// <param name="payoff">Payoff function which takes a sequence of floats
  /// </param>
  /// <param name="path">The Path instance</param>
  let calcPayoff (payoff: seq<'a>->float) path =
    match path with
    | Ordinary path -> payoff path
    | Antithetic (path, antiPath) -> 0.5 * (payoff path + payoff antiPath)

  /// <summary>
  /// Discounts a value returned from a given payoff function using  the given
  /// risk free rate and the duration in number of years
  /// </summary>
  /// <param name="r">Risk free rate</param>
  /// <param name="t">Time in years</param>
  /// <param name="payoff"></param>
  let discountedPayoff (r: float) t payoff = payoff >> ((*) (exp (-r*t)))
   
  /// <summary>
  /// Runs monte carlo simulation by first generating a Paths instance using
  /// the path generator function.  Then gather the payoffs of each Path within
  /// </summary>
  /// <param name="m">The number of paths to generate</param>
  /// <param name="genPaths">The path generator function</param>
  /// <param name="payoff">The payoff function which takes a sequence of floats
  /// as the path (i.e. not the Path type)</param>
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

  /// <summary>
  /// Parallelized version of mc.  Note that genPaths and payoff are functions
  /// which generates the path generator function and the payoff function, and
  /// not the functions themselves.  This is because if they are passed in
  /// directly, they will be shared by the different threads, which make them
  /// contention points.  Instead, pass in a function that generates them and
  /// each thread will have their own copy
  /// </summary>
  /// <param name="numBatches">Number of batches to split into</param>
  /// <param name="numPaths">Total number of paths to generate</param>
  /// <param name="genPaths">Function which generates a path generator function
  /// </param>
  /// <param name="payoff">Function which generates a payoff function</param>
  let mcpar numBatches numPaths genPaths payoff =
    let pathsPerBatch = numPaths/numBatches
    let task = async { return mc pathsPerBatch (genPaths()) (payoff()) }
    task 
    |> List.replicate numBatches 
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.unzip3 
    |> fun (a,b,c) -> (Array.average a, Array.average b, Array.average c)

  /// <summary>
  /// European payoff function
  /// </summary>
  /// <param name="optionType">Either Call or Put</param>
  /// <param name="strike">The strike price</param>
  /// <param name="path">Sequence of floats representing a path</param>
  let european optionType strike (path:seq<float>) =
    let intrinsic = path.Last() - strike
    match optionType with
    | Call -> max 0.0 intrinsic
    | Put -> max 0.0 -intrinsic

  /// <summary>
  /// Barrier option payoff function
  /// </summary>
  /// <param name="barrierUpDown">Either Up or Down</param>
  /// <param name="barrierInOut">Either In or Out</param>
  /// <param name="optionType">Either Call or Put</param>
  /// <param name="strike">The strike price</param>
  /// <param name="barrierLevel">The barrier price</param>
  /// <param name="path">Sequence of floats representing a path</param>
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

  /// <summary>
  /// Wrapper function for pricing a barrier option by calling mc
  /// </summary>
  /// <param name="s0">Initial stock price</param>
  /// <param name="r">Risk-free rate</param>
  /// <param name="q">Convenience yield</param>
  /// <param name="v">Volatility p.a.</param>
  /// <param name="t">Time to expiry in years</param>
  /// <param name="n">Number of points to generate in path</param>
  /// <param name="vr">Variance reduction scheme</param>
  /// <param name="updown">Either Up or Down</param>
  /// <param name="inout">Either In or Out</param>
  /// <param name="optType">Either Call or Put</param>
  /// <param name="strike">The strike price</param>
  /// <param name="barrierLevel">The barrier price</param>
  /// <param name="m">Number of paths to generate</param>
  let barriermc s0 r q v t n vr updown inout optType strike barrierLevel m =
    
    let payoffFunc = 
      barrier updown inout optType strike barrierLevel
      |> discountedPayoff r t

    let genPath = gbmGenPaths s0 r q v t n vr
    mc m genPath payoffFunc

  module UnitTests =

    open NUnit.Framework
    open FsUnit 
    open Payoffs.Option.UnitTests

    [<TestCase(0, 12, 34)>]
    [<TestCase(1, 56, 78)>]
    let ``Get number of Paths``(vrNum, m, n) =
      let vr = if vrNum = 0 then None else ATV
      let paths = allocatePaths vr m n
      let (r, c) = getNumOfPaths paths
      (r, c) |> should equal (m, n)

    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, 1, 5000000)>]
    let ``MC european call option with antithetic variance reduction`` 
      (s0, r, q, v, t, n, m) = 
      
      let (estimate, var, std) = 
        mc m (gbmGenPaths s0 r q v t n ATV) 
          (european Call s0 |> discountedPayoff r t)
    
      let q = 0.0
      let expected = blackScholes s0 r q v t Call s0
      estimate |> should (equalWithin (2.0 * std)) expected
  
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, 1, 5000000)>]
    let ``MC european call option without variance reduction`` 
      (s0, r, q, v, t, n, m) = 
      
      let (estimate, var, std) = 
        mc m (gbmGenPaths s0 r q v t n None) 
          (european Call s0 |> discountedPayoff r t)
    
      let q = 0.0
      let expected = blackScholes s0 r q v t Call s0
      estimate |> should (equalWithin (2.0 * std)) expected

    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 5000000, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 1000000, 3.3)>]
    let ``MC barrier option with antithetic variance reduction`` 
      ( s0, r, q, v, t, isUp, isIn, isCall, b, k, n, m, expected ) = 

      let upDown = toUpDown isUp
      let inOut = toBarrierType isIn
      let optType = toOptType isCall
      let payoff = barrier upDown inOut optType k b |> discountedPayoff r t
      let (estimate, var, std) = 
        mc m (gbmGenPaths s0 r q v t n ATV) payoff
    
      estimate |> should (equalWithin 0.01) expected

    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 1, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 2, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 3, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 4, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 5, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 10, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 20, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 25, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 50, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 100, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 200, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 500, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 1000, 3.3)>]
    [<TestCase(100.0, 0.02, 0.0, 0.4, 0.25, true, false, true, 125.0, 100.0, 3, 10000000, 2000, 3.3)>]
    let ``Parallel MC barrier option with antithetic variance reduction`` 
      ( s0, r, q, v, t, isUp, isIn, isCall, b, k, n, m, batch, expected ) = 

      let upDown = toUpDown isUp
      let inOut = toBarrierType isIn
      let optType = toOptType isCall
      let payoff() = barrier upDown inOut optType k b |> discountedPayoff r t
      let g() = gbmGenPaths s0 r q v t n ATV
      let (estimate, _, _) = mcpar batch m g payoff
      estimate |> should (equalWithin 0.01) expected