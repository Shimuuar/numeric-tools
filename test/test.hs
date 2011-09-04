
import Test.HUnit
import Text.Printf

import Numeric.Tools.Integration

-- Approximate equality
eq :: Double -> Double -> Double -> Bool
eq eps a b = abs (a - b) / min (abs a) (abs b) <= eps


----------------------------------------------------------------
-- Tests


-- Functions together with indefinite integral and list of ranges
type FunctionInt = ( Double -> Double  -- Function
                   , Double -> Double  -- Integral
                   , [(Double,Double)] -- List of ranges
                   , String            -- Name
                   )

-- Test integrator
integratorTest :: String
               -> (QuadParam -> (Double,Double) -> (Double -> Double) -> QuadRes)
               -> QuadParam
               -> FunctionInt
               -> Test
integratorTest name quad param (f,f',ranges,fname) = TestList
  [ case quadRes $ quad param (a,b) f of
      Nothing   -> TestCase $ assertFailure (printf "%s: convergence for %s failed (%f,%f)" name fname a b)
      Just appr -> 
        let exact = f' b - f' a
        in TestCase $ assertBool (printf "%s: poor convergence for %s (%f,%f) %g instead of %g"
                                    name fname a b appr exact)
                                 (eq (quadPrecision param) exact appr)
  | (a,b) <- ranges      
  ]


testIntegrators :: [Test]
testIntegrators = concat 
  [ integratorTest "Trapeze" quadTrapezoid defQuad `map` [funBlamg,funExp,funLog]
  , integratorTest "Simpson" quadSimpson   defQuad `map` [funBlamg,funExp,funLog]
  , integratorTest "Romberg" quadRomberg   defQuad `map` [funBlamg,funExp,funLog]
  ]
  where
    funBlamg = ( \x -> x^4 * log(x + sqrt (x*x + 1))
               , \x -> 1/5*x^5 * log(x + sqrt (x*x + 1)) - 1/75*sqrt(x*x+1)*(3*x^4 - 4*x^2 + 8)
               , [(0,2), (1,3), (-2,3)]
               , "x^4·log(x + sqrt(x^2 + 1))"
               )
    funExp   = ( exp, exp
               , [(0,2), (1,3), (-2,3)] 
               , "exp"
               )
    funLog   = ( log
               , \x -> x * (log x - 1)
               , [(1,2), (0.3,3)]
               , "log"
               )
