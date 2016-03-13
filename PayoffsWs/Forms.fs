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
open WebSharper.JavaScript.Dom
open WebSharper.Formlets
open WebSharper.Formlets.Controls
open PayoffsWs.Data
open Payoffs.Option

[<JavaScript>]
module Forms = 
  let floatListRegexStr = """^ *((\d*\.\d*)(\, *\d*\.\d*)*) *$"""
  let floatListSplitRegexStr = """\, *"""
  let floatListRegex = RegExp floatListRegexStr
  let floatListSplitRegex = RegExp floatListSplitRegexStr
  
  let removeCssFromFormlet tag className (formlet : Formlet<'a>) = 
    let element = formlet.Body :?> Element
    let childNodes = element.GetElementsByTagName(tag)
    for i in [ 0..(childNodes.Length - 1) ] do
      let child = childNodes.Item(i) :?> Element
      if child.HasAttribute("class") then 
        let oldClass = child.GetAttribute("class")
        let newClass = oldClass.Replace(className, "")
        child.SetAttribute("class", newClass)
    formlet
  
  let enhanceFormlet label (formlet : Formlet<'a>) = 
    formlet
    |> Enhance.WithValidationIcon
    |> Enhance.WithTextLabel label
  
  let makeNumericField typeValidateFunc defaultValue emptyWarningMsg = 
    Input defaultValue
    |> Validator.IsNotEmpty emptyWarningMsg
    |> typeValidateFunc "Please enter a number"
  
  let makeFloatListField defaultValue emptyWarningMsg = 
    Input defaultValue
    |> Validator.IsNotEmpty emptyWarningMsg
    |> Validator.Is (fun v -> 
         if v.JS.Match(floatListRegex) = null then false
         else 
           v.JS.Split(floatListSplitRegex)
           |> Array.map float
           |> Array.forall (fun f -> f > 0.0)) "A list of float"
  
  let makeNonZeroField typeValidateFunc defaultValue emptyWarningMsg = 
    makeNumericField typeValidateFunc defaultValue emptyWarningMsg 
    |> Validator.Is (fun v -> (float v) > 0.0) "Must be larger than 0"
  let makeFloatField = makeNumericField Validator.IsFloat
  let makeIntField = makeNumericField Validator.IsInt
  let makePositiveFloatField = makeNonZeroField Validator.IsFloat
  let makePositiveIntField = makeNonZeroField Validator.IsInt
  
  let ImpliedVolCalculatorFormlet(resetFunc) : Formlet<ImpliedVolQuery []> = 
    let submitButtonConf = 
      { Enhance.FormButtonConfiguration.Default with Label = Some "Calculate"
                                                     Class = 
                                                       Some "btn btn-primary" }
    
    let resetButtonConf = 
      { Enhance.FormButtonConfiguration.Default with Label = Some "Reset"
                                                     Class = 
                                                       Some "btn btn-default" }
    
    let makeFloatField = makeNumericField Validator.IsFloat
    let makeIntField = makeNumericField Validator.IsInt
    let makePositiveFloatField = makeNonZeroField Validator.IsFloat
    let makePositiveIntField = makeNonZeroField Validator.IsInt
    let s0 = 
      makePositiveFloatField "0.0" "Underlying price required" 
      |> enhanceFormlet "Underlying price"
    let r = 
      makeFloatField "0.0" "Interest rate required" 
      |> enhanceFormlet "Interest rate"
    let q = 
      makeFloatField "0.0" "Convenience yield required" 
      |> enhanceFormlet "Yield"
    let t = 
      makePositiveFloatField "0.0" "Time to expiry required" 
      |> enhanceFormlet "Time to expiry (years)"
    let k = 
      makePositiveFloatField "0.0" "Strike required" |> enhanceFormlet "Strike"
    
    let optType = 
      RadioButtonGroup (Some 0) [ "Call", Call
                                  "Put", Put ]
      |> Formlet.Horizontal
      |> Enhance.WithTextLabel "Option type"
    
    let p = 
      makeFloatListField "0.0" "Option price required" 
      |> enhanceFormlet "Option price"
    let precision = 
      makePositiveFloatField "0.0001" "Precision required" 
      |> enhanceFormlet "Precision"
    let maxIter = 
      makePositiveIntField "100" "Maximum iteration required" 
      |> enhanceFormlet "Max iteration"
    Formlet.Yield(fun s0 r q t k optType (p : string) precision maxIter -> 
      p.JS.Split(floatListSplitRegex)
      |> Array.map float
      |> Array.map (fun p_float -> 
           { s0 = float s0
             r = float r
             q = float q
             T = float t
             K = float k
             OptType = optType
             price = p_float
             precision = float precision
             maxIteration = int maxIter })) <*> s0 <*> r <*> q <*> t <*> k 
    <*> optType <*> p <*> precision <*> maxIter
    |> Enhance.WithResetAction resetFunc
    |> Enhance.WithCustomSubmitAndResetButtons submitButtonConf resetButtonConf
    |> Enhance.WithFormContainer
    |> Enhance.WithLegend "Calculate implied volatility given:"
