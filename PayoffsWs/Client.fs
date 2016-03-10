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
open WebSharper.JavaScript
open WebSharper.Html.Client
open WebSharper.Formlets
open Forms
open Data

[<JavaScript>]
module Client = 

  let Main() = 
    let resultLabel = H1 [Class "text-success"] -< [Text "0.0"]
    let setResult result = 
      match result with
      | Some s -> resultLabel.RemoveClass "text-danger"
                  resultLabel.AddClass "text-success"
                  resultLabel.Text <- s
      | None -> resultLabel.RemoveClass "text-success"
                resultLabel.AddClass "text-danger"
                resultLabel.Text <- "N/A"
    
    let resetResult() = 
      setResult <| Some "0.0"
      true

    let onSubmit (query : ImpliedVolQuery) =
      async {
        let! iv, ok = Server.calcImpliedVol query
        if ok then
          setResult <| Some (iv.ToString())
        else
          setResult None
      }
      |> Async.Start

    let form = ImpliedVolCalculatorFormlet(resetResult) |> Formlet.Run onSubmit
    Div [resultLabel] -< [form]