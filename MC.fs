namespace Payoffs

open NUnit.Framework
open FsUnit
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

module MC =

  type OptionType =
  | Call
  | Put

  type Path<'a> =
  | Ordinary of 'a[]
  | Antithetic of 'a[] * 'a[]

  type VarianceReduction =
  | None
  | ATV

  let drawNormal mean var n =
    let dist = Normal.WithMeanVariance(mean, var)
    dist.RandomSource <- new System.Random()
    fun () -> Array.init n (fun _ -> dist.Sample())
    
  let makeAntithetic theProcess =
    fun (randomArray: float[]) ->
      (theProcess <| randomArray, 
       Array.map (~-) >> theProcess <| randomArray)
      |> Path.Antithetic
    
  let gbmPaths r sigma s0 (t:float) n randomArray =
    let dt = t/float n
    let drift = (r - sigma**2.0/2.0) * dt
    randomArray
    |> Array.map (fun dwt -> drift + sigma * dwt )
    |> Array.scan (+) 0.0
    |> Array.map (fun x -> s0 * exp(x))

  let gbm r sigma s0 t n vr =
    let draw = drawNormal 0.0 (t/float n) n
    match vr with
    | None -> fun() -> draw() |> gbmPaths r sigma s0 t n |> Ordinary
    | ATV -> fun() -> gbmPaths r sigma s0 t n |> makeAntithetic <| draw()

  let genPaths n theProcess =
    Array.init n (fun _ -> theProcess())

  let discountedPayoff (r: float) t payoff = payoff >> ((*) (exp (-r*t)))

  let mc (payoff: 'a[]->float) (paths:Path<'a>[])=
    let calcPayoff path =
      match path with
      | Ordinary path -> payoff path
      | Antithetic (path, antiPath) -> 0.5 * (payoff path + payoff antiPath)
    let payoffs = paths |> Array.map calcPayoff
    let estimate = payoffs |> Array.average
    let var = payoffs.Variance()
    let stderr = sqrt (var/float payoffs.Length)
    (estimate, var, stderr)

  let european optionType strike (path:float[]) =
    let intrinsic = path.[path.Length-1] - strike
    match optionType with
    | Call -> max 0.0 intrinsic
    | Put -> max 0.0 -intrinsic