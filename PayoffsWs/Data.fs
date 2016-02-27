namespace PayoffsWs

module Data =

  open Payoffs.Option

  let OptionTypeList : (string * OptionType) list = [ "Call", Call; "Put", Put ]

  type ImpliedVolQuery = {
    s0: float
    r: float
    q: float
    T: float
    K: float
    OptType: OptionType
    price: float
    precision: float
    maxIteration: int
  }