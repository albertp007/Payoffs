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

  type Binomial(period) =
    let totalPeriods = (period+1)*(period+2)/2
    let assetPrices = Array.init totalPeriods (fun _ -> 0.0)
    let values = Array.init totalPeriods (fun _ -> 0.0)
    let states = Dictionary<int*int*int, float>()
  with
    member this.AssetPrices = assetPrices

    member this.Values = values

    member this.States = states

    member this.Period = period

    member this.ToIndex (i,j) = i * (i+1) /2 + j

    member this.GetValue (i, j) = values.[this.ToIndex(i,j)]

    member this.GetAssetPrice (i, j) = assetPrices.[this.ToIndex(i, j)]

    member this.GetState (i, j, k) = 
      if states.ContainsKey (i, j, k) then Some states.[(i, j, k)] else None

    member this.SetValue (i, j) value = values.[this.ToIndex(i, j)] <- value

    member this.SetAssetPrice(i, j) price =
      assetPrices.[this.ToIndex(i, j)] <- price

    member this.SetState (i, j, k) state =
      if states.ContainsKey(i, j, k) then states.[(i, j, k)] <- state
      else states.Add ((i, j, k), state)

    member this.GetProbabilities(r, q, v, t) =
      let dt = t / float this.Period
      let u = v * sqrt dt
      let p = (exp ((r-q)*dt) - exp(-u))/(exp (u) - exp (-u))
      let discount = exp (-r*dt)
      (exp u, p, discount)
     
  type Node = {
    mutable AssetPrice: float
    mutable Value: float
    // States: Dictionary<int, float>
  } with
    static member Make assetPrice value = 
      // { AssetPrice = assetPrice; Value = value;  States=Dictionary<int, float>() }
      { AssetPrice = assetPrice; Value = value }
    static member Default() = Node.Make 0.0 0.0

    static member SetAssetPrice (node:Node) assetPrice = 
      node.AssetPrice <- assetPrice

    static member SetValue (node:Node) value = node.Value <- value

    static member Set (node: Node) (assetPrice, value) =
      Node.SetAssetPrice node assetPrice
      Node.SetValue node value

    member this.set(assetPrice, value) = Node.Set this (assetPrice, value)

  let toIndex (i, j) = i * (i + 1) /2 + j

  let fromIndex n =
    let i = ceil (sqrt (2.0 * float n + 2.25) - 1.5)
    let j = n - int (0.5*i*(i+1.0))
    (int i, j)

  let totalNodes period = (period + 1)*(period + 2)/2

  let iterRange range f =
    for i in range do
      for j in 0..i do
        f (i, int j)

  let setRange range (grid: 'a[]) f =
    iterRange range (fun (i, j) -> grid.[toIndex(i, j)] <- f(i, j))

  let setNodes range (grid: Node[]) f =
    iterRange range (fun (i, j) -> grid.[toIndex(i,j)].set(f(i, j)))

  let setNodeValues range (grid: Node[]) f =
    iterRange range (fun (i, j) -> Node.SetValue grid.[toIndex(i,j)] (f(i, j)))

  let set n (grid: 'a[]) f = setRange [0..n] grid f
  let setPeriod n (grid: 'a[]) f = setRange [n..n] grid f
  let setRev n (grid: 'a[]) f = setRange [n..(-1)..0] grid f

  let initGrid period =
    Array.init (totalNodes period) (fun _ -> Node.Default())

  let createNode s0 up down =
    fun (i, j) ->
      (s0 * up ** (float j) * down ** (float (i-j)), 0.0)

  let calcTerminal (paths: Node[]) (payoff:Node->float) =
    fun (i, j) ->
      let n = toIndex(i, j)
      payoff paths.[n]

  let riskNeutralProb r q (v:float) dt =
    let u = v * sqrt dt
    let p = (exp ((r-q)*dt) - exp(-u))/(exp (u) - exp (-u))
    let discount = exp (-r*dt)
    (exp u, p, discount)

  let backwardInduce r q v dt nodeValue (nodes: Node[]) =
    fun (i, j) ->
      let node = nodes.[toIndex(i, j)]
      let upValue = nodes.[toIndex(i+1, j+1)].Value
      let downValue = nodes.[toIndex(i+1, j)].Value
      let (u, p, discount) = riskNeutralProb r q v dt
      nodeValue node (discount * (p*upValue + (1.0-p)*downValue))

  let price s0 r q v t n nodeValue (payoff:Node->float) =
    let nodes = initGrid n
    let dt = t/float n
    let (u, p, discount) = riskNeutralProb r q v dt
    createNode s0 u (1.0/u) |> setNodes [0..n] nodes
    calcTerminal nodes payoff |> setNodeValues [n..n] nodes
    backwardInduce r q v dt nodeValue nodes 
    |> setNodeValues [(n-1)..(-1)..0] nodes
    nodes.[0].Value

  let europeanNodeValue node discounted = discounted

  let americanNodeValue optionType strike node discounted =
    let intrinsic = node.AssetPrice - strike
    match optionType with
    | Call -> max discounted intrinsic
    | Put -> max discounted (-intrinsic)

  let vanilla optType strike node =
    let intrinsic = node.AssetPrice - strike
    match optType with
    | Call -> max 0.0 intrinsic
    | Put -> max 0.0 (-intrinsic)

  let initGrid' period =
    Array.init (totalNodes period) (fun _ -> 0.0)

  let createNode' s0 up down =
    fun (i, j) ->
      s0 * up ** (float j) * down ** (float (i - j))

  let calcTerminal' (paths: 'a[]) (payoff:'a->float) =
    fun (i, j) ->
      let n = toIndex(i, j)
      payoff paths.[n]

  let backwardInduce' r q v dt (nodes: 'a[]) =
    fun (i, j) ->
      let upValue = nodes.[toIndex(i+1, j+1)]
      let downValue = nodes.[toIndex(i+1, j)]
      let (u, p, discount) = riskNeutralProb r q v dt
      discount * (p*upValue + (1.0-p)*downValue)

  let price' s0 r q v t n (payoff:'a->float) =
    let nodes = initGrid' n
    let values = initGrid' n
    let dt = t/float n
    let (u, p, discount) = riskNeutralProb r q v dt
    createNode' s0 u (1.0/u) |> set n nodes
    calcTerminal' nodes payoff |> setPeriod n values
    backwardInduce' r q v dt values |> setRev (n-1) values
    values.[0]

  let european' optType strike s =
    let intrinsic = s - strike
    match optType with
    | Call -> max 0.0 intrinsic
    | Put -> max 0.0 (-intrinsic)
