namespace Payoffs

module Lattice =

  type OptionType =
  | Call
  | Put

  let toIndex (i, j) = i * (i + 1) /2 + j

  let fromIndex n =
    let i = ceil (sqrt (2.0 * float n + 2.25) - 1.5)
    let j = n - int (0.5*i*(i+1.0))
    (int i, j)

  let initGrid period =
    let length = (period+1) * (period+2) / 2
    Array.init length (fun _ -> 0.0)

  let iterRange range f =
    for i in range do
      for j in 0..i do
        f (i, int j)

  let setRange range (grid: 'a[]) f =
    for i in range do
      for j in 0..i do
        grid.[toIndex(i, j)] <- f(grid, i, j)

  let set n (grid: 'a[]) f = setRange [0..n] grid f
  let setPeriod n (grid: 'a[]) f = setRange [n..n] grid f
  let setRev n (grid: 'a[]) f = setRange [n..(-1)..0] grid f

  let populate s0 up down =
    fun (_, i, j) ->
      s0 * up ** (float (i - j)) * down ** (float j)

  let calcTerminal (paths: 'a[]) payoff =
    fun (_, i, j) ->
      let n = toIndex(i, j)
      payoff paths.[n]

  let riskNeutralProb r q (v:float) dt =
    let u = v * sqrt dt
    let p = (exp ((r-q)*dt) - exp(-u))/(exp (u) - exp (-u))
    let discount = exp (-r*dt)
    (exp u, p, discount)

  let backwardInduce r q v dt =
    fun (values:float[], i, j) ->
      let upValue = values.[toIndex(i+1, j)]
      let downValue = values.[toIndex(i+1, j+1)]
      let (u, p, discount) = riskNeutralProb r q v dt
      discount * (p*upValue + (1.0-p)*downValue)

  let price s0 r q v t n payoff =
    let paths = initGrid n
    let values = initGrid n
    let dt = t/float n
    let (u, p, discount) = riskNeutralProb r q v dt
    populate s0 u (1.0/u) |> set n paths
    calcTerminal paths payoff |> setPeriod n values
    backwardInduce r q v dt |> setRev (n-1) values
    values

  let european optType strike s =
    let intrinsic = s - strike
    match optType with
    | Call -> max 0.0 intrinsic
    | Put -> max 0.0 (-intrinsic)