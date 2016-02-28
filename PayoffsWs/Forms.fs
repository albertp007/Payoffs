namespace PayoffsWs

open WebSharper
open WebSharper.Formlets
open WebSharper.Formlets.Controls
open PayoffsWs.Data
open Payoffs.Option

module Forms = 
  [<JavaScript>]
  let makeField typeValidateFunc defaultValue emptyWarningMsg label = 
    Input defaultValue
    |> Validator.IsNotEmpty emptyWarningMsg
    |> typeValidateFunc "Please enter a number"
    |> Enhance.WithValidationIcon
    |> Enhance.WithTextLabel label
  
  [<JavaScript>]
  let ImpliedVolCalculatorFormlet() : Formlet<ImpliedVolQuery> = 
    let makeFloatField = makeField Validator.IsFloat
    let makeIntField = makeField Validator.IsInt
    let s0 = makeFloatField "0.0" "Underlying price required" "Underlying price"
    let r = makeFloatField "0.0" "Interest rate required" "Interest rate"
    let q = makeFloatField "0.0" "Convenience yield required" "Yield"
    let t = 
      makeFloatField "0.0" "Time to expiry required" "Time to expiry (years)"
    let k = makeFloatField "0.0" "Strike required" "Strike"
    
    let optType = 
      Select 0 [ "Call", Call
                 "Put", Put ]
      |> Enhance.WithTextLabel "Option type"
    
    let p = makeFloatField "0.0" "Option price required" "Option price"
    let precision = makeFloatField "0.0001" "Precision required" "Precision"
    let maxIter = 
      makeIntField "100" "Maximum iteration required" "Max iteration"
    Formlet.Yield(fun s0 r q t k optType p precision maxIter -> 
      { s0 = float s0
        r = float r
        q = float q
        T = float t
        K = float k
        OptType = optType
        price = float p
        precision = float precision
        maxIteration = int maxIter }) <*> s0 <*> r <*> q <*> t <*> k <*> optType 
    <*> p <*> precision <*> maxIter
    |> Enhance.WithSubmitAndResetButtons
    |> Enhance.WithFormContainer
    |> Enhance.WithLegend "Calculate implied volatility given:"
