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
    let f = ImpliedVolCalculatorFormlet()
    Formlet.Run 
      (fun (query : ImpliedVolQuery) -> 
        async {
          let! iv, ok = Server.calcImpliedVol query
          if ok then
            JS.Alert <| sprintf "Implied volatility: %f" iv
          else
            JS.Alert <| sprintf "Implied volatility: Cannot solve for IV given the inputs"
        }
        |> Async.Start
      )
      f
