namespace PayoffsWs

open WebSharper
open Data
open Payoffs.Option

module Server = 
  [<Rpc>]
  let DoSomething input = 
    let R(s : string) = System.String(Array.rev (s.ToCharArray()))
    async { return R input }
  
  [<Rpc>]
  let calcImpliedVol (query : ImpliedVolQuery) = 
    async 
      { 
        let iv, _ = impliedVolatility query.precision query.maxIteration query.s0 query.r query.q query.T query.K query.OptType query.price
        return if iv <> iv then -1.0, false else iv, true
      }
