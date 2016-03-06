namespace PayoffsWs

open WebSharper
open WebSharper.JavaScript
open WebSharper.JavaScript.Dom
open WebSharper.Formlets
open WebSharper.Formlets.Controls
open PayoffsWs.Data
open Payoffs.Option

[<JavaScript>]
module Forms = 

  let removeCssFromFormlet tag className (formlet:Formlet<'a>) =
    let element = formlet.Body :?> Element
    let childNodes = element.GetElementsByTagName(tag)
    for i in [0..(childNodes.Length-1)] do
      let child = childNodes.Item(i) :?> Element
      if child.HasAttribute("class") then
        let oldClass = child.GetAttribute("class")
        let newClass = oldClass.Replace(className, "")
        child.SetAttribute("class", newClass)
    formlet

  let enhanceFormlet label (formlet:Formlet<'a>) =
    formlet
    |> Enhance.WithValidationIcon
    |> Enhance.WithTextLabel label

  let makeField typeValidateFunc defaultValue emptyWarningMsg = 
    Input defaultValue
    |> Validator.IsNotEmpty emptyWarningMsg
    |> typeValidateFunc "Please enter a number"

  let makeNonZeroField typeValidateFunc defaultValue emptyWarningMsg =
    makeField typeValidateFunc defaultValue emptyWarningMsg
    |> Validator.Is (fun v -> (float v) > 0.0 ) "Must be larger than 0"

  let makeFloatField = makeField Validator.IsFloat
  let makeIntField = makeField Validator.IsInt
  let makePositiveFloatField = makeNonZeroField Validator.IsFloat
  let makePositiveIntField = makeNonZeroField Validator.IsInt

  let ImpliedVolCalculatorFormlet(resetFunc) : Formlet<ImpliedVolQuery> = 
    let submitButtonConf = { 
      Enhance.FormButtonConfiguration.Default with 
        Label = Some "Calculate"; Class = Some "btn btn-primary" 
    }
    let resetButtonConf = { 
      Enhance.FormButtonConfiguration.Default with
        Label = Some "Reset"; Class = Some "btn btn-default"
    } 
    let makeFloatField = makeField Validator.IsFloat
    let makeIntField = makeField Validator.IsInt
    let makePositiveFloatField = makeNonZeroField Validator.IsFloat
    let makePositiveIntField = makeNonZeroField Validator.IsInt

    let s0 = makePositiveFloatField "0.0" "Underlying price required" 
             |> enhanceFormlet "Underlying price"
             
    let r = makeFloatField "0.0" "Interest rate required" 
            |> enhanceFormlet "Interest rate"

    let q = makeFloatField "0.0" "Convenience yield required" 
            |> enhanceFormlet "Yield"

    let t = makePositiveFloatField "0.0" "Time to expiry required" 
            |> enhanceFormlet "Time to expiry (years)"

    let k = makePositiveFloatField "0.0" "Strike required" 
            |> enhanceFormlet "Strike"
    
    let optType = 
      RadioButtonGroup (Some 0) [ "Call", Call; "Put", Put ]
      |> Formlet.Horizontal
      |> Enhance.WithTextLabel "Option type"
    
    let p = makePositiveFloatField "0.0" "Option price required" 
            |> enhanceFormlet "Option price"

    let precision = makePositiveFloatField "0.0001" "Precision required" 
                    |> enhanceFormlet "Precision"
    let maxIter = makePositiveIntField "100" "Maximum iteration required" 
                  |> enhanceFormlet "Max iteration"

    Formlet.Yield(fun s0 r q t k optType p precision maxIter -> 
          { s0 = float s0
            r = float r
            q = float q
            T = float t
            K = float k
            OptType = optType
            price = float p
            precision = float precision
            maxIteration = int maxIter }) 
    <*> s0 <*> r <*> q <*> t <*> k <*> optType 
    <*> p <*> precision <*> maxIter
    |> Enhance.WithResetAction resetFunc
    |> Enhance.WithCustomSubmitAndResetButtons submitButtonConf resetButtonConf
    |> Enhance.WithFormContainer
    |> Enhance.WithLegend "Calculate implied volatility given:"