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
    let values = Array.init numNodes (fun _ -> 0.0)
    let states = Dictionary<int*int*int, float>()
    let dt = t / float period
  with
    static member IterRange range f =
      for i in range do
        for j in 0..i do
          f (i, int j)

    static member NodeAssetPrice s0 up down =
      fun _ (i, j) -> s0 * up ** (float j) * down ** (float (i-j))

    member this.AssetPrices = assetPrices

    member this.Values = values

    member this.States = states

    member this.Period = period

    member this.InitialAssetPrice = s0

    member this.r = r

    member this.q = q

    member this.v = v

    member this.t = t

    member this.NumNodes = numNodes

    member this.ToIndex (i,j) = i * (i+1) /2 + j

    member this.GetValue (i, j) = values.[this.ToIndex(i,j)]

    member this.GetAssetPrice (i, j) = assetPrices.[this.ToIndex(i, j)]

    member this.GetState (i, j, k) = 
      if states.ContainsKey (i, j, k) then Some states.[(i, j, k)] else None

    member this.SetValue (i, j) value = values.[this.ToIndex(i, j)] <- value

    member this.SetAssetPrice (i, j) price =
      assetPrices.[this.ToIndex(i, j)] <- price

    member this.SetState (i, j, k) state =
      if states.ContainsKey(i, j, k) then states.[(i, j, k)] <- state
      else states.Add ((i, j, k), state)

    member this.SetAssetPrices f =
      Binomial.IterRange [0..this.Period] (fun (i, j) -> 
        this.SetAssetPrice (i, j) (f this (i, j)))

    member this.SetValues range f =
      Binomial.IterRange range (fun (i, j) -> 
        this.SetValue (i, j) (f this (i, j)))

    member this.GetProbabilities() =
      let u = v * sqrt dt
      let p = (exp ((r-q)*dt) - exp(-u))/(exp (u) - exp (-u))
      let discount = exp (-r*dt)
      (exp u, p, discount)

    member this.GetInducedValue(i, j) =
      let (_, p, discount) = this.GetProbabilities()
      let upValue = this.GetValue (i+1, j+1)
      let downValue = this.GetValue (i+1, j)
      let (_, p, discount) = this.GetProbabilities()
      (discount * (p*upValue + (1.0-p)*downValue))

    member this.GetIntrinsic optType strike (i, j) =
      let intrinsic = this.GetAssetPrice(i, j) - strike
      match optType with
      | Call -> intrinsic
      | Put -> (-intrinsic)

    member this.BuildGrid() = 
      let (up, _, _) = this.GetProbabilities()
      this.SetAssetPrices <| Binomial.NodeAssetPrice s0 up (1.0/up)

    member this.SetTerminal terminalFunc =
      let n = this.Period
      this.SetValues [n..n] terminalFunc

    member this.InduceBackward nodeValueFunc =
      let n = this.Period
      this.SetValues [(n-1)..(-1)..0] nodeValueFunc
    
    member this.Price terminalFunc nodeValueFunc =
      this.BuildGrid()
      terminalFunc |> this.SetTerminal
      nodeValueFunc |> this.InduceBackward
      this.GetValue (0, 0)
     
  let fromIndex n =
    let i = ceil (sqrt (2.0 * float n + 2.25) - 1.5)
    let j = n - int (0.5*i*(i+1.0))
    (int i, j)

  let vanillaPayoff optType strike = 
    fun (tree:Binomial) (i, j) ->
      let intrinsic = tree.GetIntrinsic optType strike (i, j)
      max 0.0 intrinsic

  let terminal optType strike = 
    fun (tree:Binomial) (i, j) ->
      let intrinsic = tree.GetAssetPrice (i, j) - strike
      match optType with 
      | Call -> max 0.0 intrinsic
      | Put -> max 0.0 (-intrinsic)

  let europeanValue =
    fun (tree:Binomial) (i, j) ->
      tree.GetInducedValue(i, j)

  let americanValue optType strike =
    fun (tree:Binomial) (i, j) ->
      let induced = tree.GetInducedValue(i, j)
      let current = tree.GetAssetPrice(i, j)
      let intrinsic = tree.GetIntrinsic optType strike (i, j)
      max intrinsic induced
