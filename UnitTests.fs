namespace Payoffs

module UnitTests =

  open NUnit.Framework
  open FsUnit
  open Payoffs.MC
  open MathNet.Numerics.Statistics

  [<TestCase(0.0, 1.0, 1000000)>]
  let ``Draw samples from normal distribution``(mu, sigma, size) =
    let draw = drawNormal mu sigma size
    let samples = draw()
    samples.Variance() |> should (equalWithin 0.1) 1.0
    samples.Mean() |> should (equalWithin 0.1) 0.0

  [<TestCase(0.02, 0.4, 100.0, 0.25, 1, 5000000)>]
  let ``Pricing european call option with antithetic variance reduction``(r, sigma, s0, t, n, m) =
    let (estimate, var, std) =
      gbm r sigma s0 t n ATV
      |> genPaths m 
      |> mc (european Call s0 |> discountedPayoff r t)
    estimate |> should (equalWithin (2.0*std)) 8.19755

  [<TestCase(0.02, 0.4, 100.0, 0.25, 1, 5000000)>]
  let ``Pricing european call option without variance reduction``(r, sigma, s0, t, n, m) =
    let (estimate, var, std) =
      gbm r sigma s0 t n None
      |> genPaths m 
      |> mc (european Call s0 |> discountedPayoff r t)
    estimate |> should (equalWithin (2.0*std)) 8.19755


