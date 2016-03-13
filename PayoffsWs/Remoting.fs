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
  let calcImpliedVol (queries : ImpliedVolQuery []) = 
    async { 
      return queries
             |> Array.map 
                  (fun query -> 
                  impliedVolatility query.precision query.maxIteration query.s0 
                    query.r query.q query.T query.K query.OptType query.price)
             |> Array.map (fun (res, _) -> 
                  if res <> res then (-1.0, false)
                  else (res, true))
    }
