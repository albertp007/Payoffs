namespace Payoffs

  module Data =

    type Bar = {
      d: System.DateTime
      h: float
      l: float
      o: float
      c: float
      v: int64
      adj: float option
    }

