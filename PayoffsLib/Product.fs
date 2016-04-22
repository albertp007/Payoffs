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
open Payoffs.Lattice
open System.Collections.Generic

module Product = 
  module Lattice = 
    let vanillaPayoff optType strike = 
      fun (tree : Binomial) (i, j, k) -> 
        let intrinsic = tree.GetIntrinsic optType strike (i, j)
        max 0.0 intrinsic
    
    let vanillaStateFunc (i, j) k to_j = 0

    let europeanValue = 
      fun (tree : Binomial) (i, j, k) -> 
        tree.GetInducedValue vanillaStateFunc (i, j, k)
    
    let americanValue optType strike = 
      fun (tree : Binomial) (i, j, k) -> 
        let induced = tree.GetInducedValue vanillaStateFunc (i, j, k)
        let intrinsic = tree.GetIntrinsic optType strike (i, j)
        max intrinsic induced
    
    let lookbackPutState (i, j) k to_j = max k to_j
    
    let lookbackPutStatePrice (tree : Binomial) state = 
      let factor = 
        if (state > 0) then tree.Up
        else 1.0 / tree.Up
      tree.InitialAssetPrice * (factor ** float state)
    
    let lookbackPutPayoff (tree : Binomial) (i, j, k) = 
      let nodePrice = tree.GetAssetPrice(i, j)
      let maxPrice = lookbackPutStatePrice tree k
      max 0.0 (maxPrice - nodePrice)
    
    let lookbackPutValue exerciseType = 
      fun (tree : Binomial) (i, j, k) -> 
        let induced = tree.GetInducedValue lookbackPutState (i, j, k)
        match exerciseType with
        | European -> induced
        | American -> 
          let assetPrice = tree.GetAssetPrice(i, j)
          let maxPrice = lookbackPutStatePrice tree k
          max induced (maxPrice - assetPrice)

    let barrierTrigger direction triggerPrice =
      let compOp = match direction with
                   | Up -> (>=)
                   | Down -> (<=)
      fun assetPrice ->
        compOp assetPrice triggerPrice

    let barrierState direction (tree : Binomial) koPrice = 
      let isTriggered = barrierTrigger direction koPrice
      fun (i, j) k to_j -> 
        // state 1 - triggered
        // state 0 - not triggered
        if k = 1 then 1 // if already triggered, then continue to be triggered
        else
          let nodePrice = tree.GetAssetPrice(i + 1, to_j)
          if isTriggered nodePrice then 1 else 0

    let barrierPayoff barrierType optType strike =
      fun (tree: Binomial) (i, j, k) ->
        let isNoValue = 
          match barrierType with
          | In -> k = 0 // not triggered i.e. not knocked in
          | Out -> k = 1 // triggered, i.e. knocked out
        if isNoValue then 0.0
        else
          let intrinsic = tree.GetIntrinsic optType strike (i, j)
          max 0.0 intrinsic
    
    let barrierValue (tree: Binomial) =
      let stateFunc (i, j) k to_j =
        if k = 1 then 1
        else
          let states = tree.GetStates(i+1, to_j)
          if states.ContainsKey 0 then 0 
          else 1
          
      fun (tree:Binomial) (i, j, k) ->
        tree.GetInducedValue stateFunc (i, j, k)

    module UnitTests =

      open NUnit.Framework
      open FsUnit
      open Payoffs.Option.UnitTests

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
            (fun count (table : Dictionary<int, float>) -> 
              count + table.Count) 0 tables
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
        List.forall (fun state -> 
          containsKey tree.StateValues state) correctStates |> should equal true
  
      [<TestCase(50.0, 0.1, 0.0, 0.4, 0.25, 3)>]
      let ``Binomial Lookback Put`` (s0, r, q, v, t, n) = 
        let tree = Binomial(s0, r, q, v, t, n)
        let price = tree.Price lookbackPutState lookbackPutPayoff 
                      (lookbackPutValue American)
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