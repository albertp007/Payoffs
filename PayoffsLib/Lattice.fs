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

  /// <summary>
  /// Type representing a binomial tree with methods for pricing path-dependent
  /// options using the forward shooting grid method, which stores a state
  /// vector for each node and a state transition function which tracks the
  /// evolution of the states when going from a node in a particular level to
  /// its up node and the down node in the next period.  The nodes of the tree
  /// is stored in a flat array, with its elements being the underlying price
  /// at that node.  The (i, j) node is the node in the i-th period with j
  /// number of up moves from the initial price.  j can be negative in which
  /// case it means j number of down moves.  (0, 0) is the root node.  For
  /// example, a tree with 3 periods have nodes (0, 0) as root node, node
  /// (1, -1) and (1, 1) in the first period and (2, -2), (2, 0) and (2, 2) in
  /// the second period; (3, -3), (3, -1), (3, 1) and (3, 3) in the 3rd period.
  /// In general, the nodes in the n-th period are (n, -n), (n, -n+2), ..., 
  /// (n, -n + 2i), ..., (n, n)
  /// </summary>
  type Binomial(s0, r, q, v, t, period) =

    let numNodes = (period+1)*(period+2)/2

    let assetPrices: float[] = Array.zeroCreate numNodes

    // An array of dictionaries with the state as the key and the value of the
    // node as value
    let stateValues = Array.init numNodes (fun _ -> Dictionary<int, float>())

    let dt = t / float period

    let u = v * sqrt dt

  with
    /// <summary>
    /// Generates the indices of a binomial tree given the range which 
    /// represents the start period and the end period then passes each of the
    /// index to a function f
    /// </summary>
    /// <param name="range">Range of the start period and end period to
    /// generate indices for</param>
    /// <param name="f">Unit function that is passed each of the node index
    /// generated</param>
    static member IterRange range f =
      for i in range do
        for j in [(-i)..(2)..i] do
          f (i, int j)

    /// <summary>
    /// Constructs a function which takes in the node index (i, j) and returns
    /// the underlying price at that node index
    /// </summary>
    /// <param name="s0">Initial stock price</param>
    /// <param name="up">The up factor when going up from a node</param>
    /// <param name="down">The down factor when going down from a node</param>
    static member NodeAssetPrice s0 up down =
      fun _ (i, j) -> 
        let (factor, j') = (if j > 0 then up, float j else down, float (-j))
        s0 * (factor ** j')

    /// <summary>
    /// Calculates the node index (i, j) given the index of the element in the
    /// raw array representing the tree
    /// </summary>
    /// <param name="n">Index in the raw array</param>
    static member FromIndex n =
      let i = ceil (sqrt (2.0 * float n + 2.25) - 1.5)
      let j = n - int (0.5*i*(i+1.0))
      (int i, j)

    member this.AssetPrices = assetPrices

    member this.StateValues = stateValues

    member this.Period = period

    member this.InitialAssetPrice = s0

    member this.Up = exp (u)

    /// <summary>
    /// Calculates the risk-neutral probability of an up move
    /// </summary>
    member this.Prob = 
      (exp ((r-q)*dt) - exp(-u))/(exp (u) - exp (-u))

    /// <summary>
    /// Calculates the discount factor of one time period
    /// </summary>
    member this.Discount = exp (-r*dt)

    member this.NumNodes = numNodes

    /// <summary>
    /// Calculates the index of the node in the raw array given the node index
    /// (i, j) in the binomial tree
    /// </summary>
    /// <param name="i">period number</param>
    /// <param name="j">number of up/down moves from initial node</param>
    member this.ToIndex (i,j) = 
      // i * (i+1) /2 + (i + j)/2
      (i*i + 2*i + j)/2

    /// <summary>
    /// Returns the asset price of a node (i, j)
    /// </summary>
    /// <param name="i">period number</param>
    /// <param name="j">number of up/down moves from initial node</param>
    member this.GetAssetPrice (i, j) = assetPrices.[this.ToIndex(i, j)]

    /// <summary>
    /// Returns the value of the node (i, j) in state k
    /// </summary>
    /// <param name="i">period number</param>
    /// <param name="j">number of up/down moves from initial node</param>
    /// <param name="k">state</param>
    member this.GetStateValue (i, j, k) = stateValues.[this.ToIndex(i, j)].[k]

    /// <summary>
    /// Returns the state/value dictionary of the node (i, j)
    /// </summary>
    /// <param name="i">period number</param>
    /// <param name="j">number of up/down moves from initial node</param>
    member this.GetStates (i, j) =
      stateValues.[this.ToIndex(i, j)]

    /// <summary>
    /// Sets the underlying price of node (i, j)
    /// </summary>
    /// <param name="i">period number</param>
    /// <param name="j">number of up/down moves from initial node</param>
    /// <param name="price">price to set into the node</param>
    member this.SetAssetPrice (i, j) price =
      assetPrices.[this.ToIndex(i, j)] <- price

    /// <summary>
    /// Sets the value of the node (i, j) in state k
    /// </summary>
    /// <param name="i">period number</param>
    /// <param name="j">number of up/down moves from initial node</param>
    /// <param name="k">state</param>
    /// <param name="value">value to set into the node at state k</param>
    member this.SetStateValue (i, j, k) value =
      let values = stateValues.[this.ToIndex(i,j)]
      if values.ContainsKey(k) then 
        values.[k] <- value
      else 
        values.Add (k, value)

    /// <summary>
    /// Populates the underlying price of all the nodes in the tree by the
    /// given function which takes the tree and the node index (i, j) as
    /// parameters and returns a price
    /// </summary>
    /// <param name="f">Node price calculation function</param>
    member this.SetAssetPrices f =
      Binomial.IterRange [0..this.Period] (fun (i, j) -> 
        this.SetAssetPrice (i, j) (f this (i, j)))

    /// <summary>
    /// Iterates through the nodes of the tree from the root node.  For each
    /// node, look at the existing states and pass each of the state at (i, j)
    /// to function f which returns its value.  The returned values are then
    /// set into the state vector of that node.
    /// </summary>
    /// <param name="range">Range of period to iterate through</param>
    /// <param name="f">Value calculation function which takes the binomial
    /// tree, the node index (i, j) and the state k as arguments</param>
    member this.SetStateValues range f =
      let g (i, j) =
        let stateTable = this.GetStates(i, j)
        let keys = stateTable.Keys |> Seq.toList
        List.iter (fun k-> stateTable.[k] <- f this (i, j, k)) keys
      Binomial.IterRange range g

    /// <summary>
    /// Calculate the backward-induced value of each node from the values and
    /// states of its up node and down node in the next period.  The states in
    /// the up node and down node are governed by the state transition function
    /// given the current state k at node (i, j).  Basically,
    ///
    /// V(i, j, k) = p * V(i+1, j+1, s((i,j), k, j+1)) +
    ///             (1-p) * V(i+1, j-1, s((i,j), k, j-1))
    ///
    /// where s((i, j), k, to_j) is the state transition function and p is the
    /// risk neutral probability of an up move
    /// </summary>
    /// <param name="stateFunc">State transition function</param>
    /// <param name="i">Period i</param>
    /// <param name="j">j up/down moves from the initial price</param>
    /// <param name="k">state k</param>
    member this.GetInducedValue stateFunc (i, j, k) =
      let upK = stateFunc (i, j) k (j+1)
      let downK = stateFunc (i, j) k (j-1)
      let upValue = this.GetStateValue (i+1, j+1, upK)
      let downValue = this.GetStateValue (i+1, j-1, downK)
      (this.Discount * (this.Prob*upValue + (1.0-this.Prob)*downValue))

    /// <summary>
    /// Calculates intrinsic value of the node based on a strike price and
    /// the underlying asset price of the node
    /// </summary>
    /// <param name="optType">Either call or put</param>
    /// <param name="strike">The strike price</param>
    /// <param name="i">Period i</param>
    /// <param name="j">j up/down moves from initial price</param>
    member this.GetIntrinsic optType strike (i, j) =
      let intrinsic = this.GetAssetPrice(i, j) - strike
      match optType with
      | Call -> intrinsic
      | Put -> (-intrinsic)

    /// <summary>
    /// Builds the binomial tree by populating the underlying asset price of
    /// all nodes
    /// </summary>
    member this.BuildGrid() = 
      this.SetAssetPrices <| Binomial.NodeAssetPrice s0 this.Up (1.0/this.Up)

    /// <summary>
    /// Calculates the terminal value of all the nodes in the period at expiry,
    /// given a terminal value calculation function.  For each terminal node and
    /// for each possible state in the node, the node index (i, j) and the state
    /// k are passed to the function.
    /// </summary>
    /// <param name="terminalFunc">Function which takes a triplet (i, j, k)
    /// for the node index (i, j) at state k and returns a value</param>
    member this.SetTerminal terminalFunc = 
      let n = this.Period
      this.SetStateValues [n..n] terminalFunc

    /// <summary>
    /// Starts the backward induction process given a node value calculation
    /// function, which returns the node value of node (i, j) for each of its
    /// states
    /// </summary>
    /// <param name="nodeValueFunc">Function which takes a triplet (i, j, k)
    /// for the node index (i, j) at state k and returns a value</param>
    member this.InduceBackward nodeValueFunc =
      let n = this.Period
      this.SetStateValues [(n-1)..(-1)..0] nodeValueFunc
    
    /// <summary>
    /// Starts the pricing process by
    /// <para> 1. Building the forward shooting grid by traversing each node
    /// foward</para>
    /// <para> 2. Starting the backward-induction period from the period at
    /// at expiry, traversing each node in the reverse direction, calculating
    /// its values for each of its possible states</para>
    /// <para> 3. Returning the price of the root node</para>
    /// </summary>
    /// <param name="stateFunc">State transition function</param>
    /// <param name="terminalFunc">Terminal value calculation function</param>
    /// <param name="nodeValueFunc">Node value calculation function</param>
    member this.Price stateFunc terminalFunc nodeValueFunc =
      this.BuildFSG stateFunc
      terminalFunc |> this.SetTerminal
      nodeValueFunc |> this.InduceBackward
      this.GetStateValue (0, 0, 0)
    
    /// <summary>
    /// Calculates the underlying asset price of the tree at node (i, j)
    /// </summary>
    /// <param name="tree">The binomial tree</param>
    /// <param name="i">Period i</param>
    /// <param name="j">j up/down moves from the initial price</param>
    member this.CalcAssetPrice (tree, i, j) =
      let nodeAssetPrice = Binomial.NodeAssetPrice s0 this.Up (1.0/this.Up)
      nodeAssetPrice this (i, j)

    /// <summary>
    /// Populates the state vector of the up node and the down node from the
    /// current node (i, j) and for each of the states in (i, j) using the state
    /// transition function passed in.  The state transition function is
    /// <para>newUpState = g((i, j), k, j+1) when in state k of the current node
    /// (i, j) going up to the up node in period i+1</para>
    /// <para>downState = g((i, j), k, j-1) when in state k of the current node
    /// (i, j) going down to the down node in period i+1</para>
    /// </summary>
    /// <param name="stateFunc">State transition function</param>
    /// <param name="i">Period i</param>
    /// <param name="j">j up/down moves from the initial price</param>
    member this.ForwardFrom stateFunc (i, j) =
      let upNode = (i+1, j+1)
      let downNode = (i+1, j-1)
      let states = this.StateValues.[this.ToIndex(i,j)].Keys
      for k in states do
        this.SetStateValue (i+1, j+1, stateFunc (i, j) k (j+1)) 0.0
        this.SetStateValue (i+1, j-1, stateFunc (i, j) k (j-1)) 0.0

    /// <summary>
    /// Builds the forward shooting grid by first populating the underlying
    /// asset price of each node, then run state transition function for each
    /// of the nodes in the forward direction, period by period
    /// </summary>
    /// <param name="stateFunc">State transition function</param>
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