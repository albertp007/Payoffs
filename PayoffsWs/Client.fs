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