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
    
    let koState direction (tree : Binomial) koPrice = 
      let compOp = match direction with
                   | Up -> (>=)
                   | Down -> (<=)
      fun (i, j) k to_j -> 
        if k = 1 then 1
        else
          let nodePrice = tree.GetAssetPrice(i + 1, to_j)
          if compOp nodePrice koPrice then 1
          else 0

    let koPayoff optType strike =
      fun (tree: Binomial) (i, j, k) ->
        if k = 1 then 0.0
        else
          let intrinsic = tree.GetIntrinsic optType strike (i, j)
          max 0.0 intrinsic
    
    let koValue (tree: Binomial) =
      let stateFunc (i, j) k to_j =
        if k = 1 then 1
        else
          let states = tree.GetStates(i+1, to_j)
          if states.ContainsKey 0 then 0 else 1
          
      fun (tree:Binomial) (i, j, k) ->
        tree.GetInducedValue stateFunc (i, j, k)