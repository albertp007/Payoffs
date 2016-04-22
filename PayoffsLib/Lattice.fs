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

open Payoffs.Option
open System.Collections.Generic

module Lattice =

  type Binomial(s0, r, q, v, t, period) =
    let numNodes = (period+1)*(period+2)/2
    let assetPrices = Array.init numNodes (fun _ -> 0.0)
    // let values = Array.init numNodes (fun _ -> 0.0)
    let stateValues = Array.init numNodes (fun _ -> Dictionary<int, float>())
    let dt = t / float period
    let u = v * sqrt dt

  with
    static member IterRange range f =
      for i in range do
        for j in [(-i)..(2)..i] do
          f (i, int j)

    static member NodeAssetPrice s0 up down =
      fun _ (i, j) -> 
        let (factor, j') = (if j > 0 then up, float j else down, float (-j))
        s0 * (factor ** j')

    static member FromIndex n =
      let i = ceil (sqrt (2.0 * float n + 2.25) - 1.5)
      let j = n - int (0.5*i*(i+1.0))
      (int i, j)

    member this.AssetPrices = assetPrices

    // member this.Values = values

    member this.StateValues = stateValues

    member this.Period = period

    member this.InitialAssetPrice = s0

    member this.Up = exp (u)

    member this.Prob = 
      (exp ((r-q)*dt) - exp(-u))/(exp (u) - exp (-u))

    member this.Discount = exp (-r*dt)

    member this.NumNodes = numNodes

    member this.ToIndex (i,j) = 
      // i * (i+1) /2 + (i + j)/2
      (i*i + 2*i + j)/2

    // member this.GetValue (i, j) = values.[this.ToIndex(i,j)]

    member this.GetAssetPrice (i, j) = assetPrices.[this.ToIndex(i, j)]

    member this.GetStateValue (i, j, k) = stateValues.[this.ToIndex(i, j)].[k]

    // member this.SetValue (i, j) value = values.[this.ToIndex(i, j)] <- value

    member this.GetStates (i, j) =
      stateValues.[this.ToIndex(i, j)]

    member this.SetAssetPrice (i, j) price =
      assetPrices.[this.ToIndex(i, j)] <- price

    member this.SetStateValue (i, j, k) value =
      let values = stateValues.[this.ToIndex(i,j)]
      if values.ContainsKey(k) then 
        values.[k] <- value
      else 
        values.Add (k, value)

    member this.SetAssetPrices f =
      Binomial.IterRange [0..this.Period] (fun (i, j) -> 
        this.SetAssetPrice (i, j) (f this (i, j)))

    // member this.SetValues range f =
    //  Binomial.IterRange range (fun (i, j) -> 
    //    this.SetValue (i, j) (f this (i, j)))

    member this.SetStateValues range f =
      let g (i, j) =
        let stateTable = this.GetStates(i, j)
        let keys = stateTable.Keys |> Seq.toList
        List.iter (fun k-> stateTable.[k] <- f this (i, j, k)) keys
      Binomial.IterRange range g

    member this.GetInducedValue stateFunc (i, j, k) =
      let upK = stateFunc (i, j) k (j+1)
      let downK = stateFunc (i, j) k (j-1)
      let upValue = this.GetStateValue (i+1, j+1, upK)
      let downValue = this.GetStateValue (i+1, j-1, downK)
      (this.Discount * (this.Prob*upValue + (1.0-this.Prob)*downValue))

    member this.GetIntrinsic optType strike (i, j) =
      let intrinsic = this.GetAssetPrice(i, j) - strike
      match optType with
      | Call -> intrinsic
      | Put -> (-intrinsic)

    member this.BuildGrid() = 
      this.SetAssetPrices <| Binomial.NodeAssetPrice s0 this.Up (1.0/this.Up)

    member this.SetTerminal terminalFunc = 
      let n = this.Period
      this.SetStateValues [n..n] terminalFunc

    member this.InduceBackward nodeValueFunc =
      let n = this.Period
      this.SetStateValues [(n-1)..(-1)..0] nodeValueFunc
    
    member this.Price stateFunc terminalFunc nodeValueFunc =
      this.BuildFSG stateFunc
      terminalFunc |> this.SetTerminal
      nodeValueFunc |> this.InduceBackward
      this.GetStateValue (0, 0, 0)
    
    member this.CalcAssetPrice (tree, i, j) =
      let nodeAssetPrice = Binomial.NodeAssetPrice s0 this.Up (1.0/this.Up)
      nodeAssetPrice this (i, j)

    member this.ForwardFrom stateFunc (i, j) =
      let upNode = (i+1, j+1)
      let downNode = (i+1, j-1)
      let states = this.StateValues.[this.ToIndex(i,j)].Keys
      for k in states do
        this.SetStateValue (i+1, j+1, stateFunc (i, j) k (j+1)) 0.0
        this.SetStateValue (i+1, j-1, stateFunc (i, j) k (j-1)) 0.0

    member this.BuildFSG stateFunc =
      this.SetAssetPrice (0, 0) s0
      this.SetStateValue (0, 0, 0) 0.0
      this.SetAssetPrices <| Binomial.NodeAssetPrice s0 this.Up (1.0/this.Up)
      Binomial.IterRange [0..(this.Period-1)] <| this.ForwardFrom stateFunc
     
  module UnitTests =

    open NUnit.Framework
    open FsUnit
    open Payoffs.Option.UnitTests

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