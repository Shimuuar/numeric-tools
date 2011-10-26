
import Test.HUnit
import Text.Printf

import Numeric.Tools.Integration
import Numeric.Tools.Differentiation
import Numeric.Tools.Equation
import Numeric.ApproxEq


----------------------------------------------------------------
-- Integration
----------------------------------------------------------------

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
               -> [Test]
integratorTest name quad param (f,f',ranges,fname) =
  [ case quadRes $ quad param (a,b) f of
      Nothing   -> TestCase $ assertFailure (printf "%s: convergence for %s failed (%f,%f)" name fname a b)
      Just appr -> 
        let exact = f' b - f' a
        in TestCase $ assertBool (printf "%s: poor convergence for %s (%f,%f) %g instead of %g"
                                    name fname a b appr exact)
                                 (eqRelative (quadPrecision param) exact appr)
  | (a,b) <- ranges      
  ]


testIntegration :: [Test]
testIntegration = concat 
  [ integratorTest "Trapeze" quadTrapezoid defQuad =<< [funBlamg,funExp,funLog]
  , integratorTest "Simpson" quadSimpson   defQuad =<< [funBlamg,funExp,funLog]
  , integratorTest "Romberg" quadRomberg   defQuad =<< [funBlamg,funExp,funLog]
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



----------------------------------------------------------------
--
----------------------------------------------------------------

type FunctionDiff = ( Double -> Double  -- Function
                    , Double -> Double  -- Derivative
                    , [(Double,Double)] -- Points and delta to evaluate
                    , String            -- Name
                    )

differentiationTest :: String
                    -> ((Double -> Double) -> Double -> Double -> DiffRes)
                    -> FunctionDiff
                    -> [Test]
differentiationTest name diff (f,f',xs,fname) = 
  [ let DiffRes appr err = diff f h x
        exact            = f' x
    in TestCase $ 
         assertBool
         (printf "%s: poor precision for %s, got %g instead of %g" name fname appr exact)
         (eqRelative 1e-13 appr exact)
  | (x,h) <- xs
  ]

testDifferentiation :: [Test]
testDifferentiation = concat
  [ differentiationTest "richardson" diffRichardson =<< [funSqr,funExp,funLog]
  ]
  where
    funSqr = ( \x -> x*x
             , \x -> 2*x
             , zip ([-10 .. -1]++[1..10]) (repeat 1)
             , "square"
             )
    funExp = ( exp, exp
             , zip [-10..10] (repeat 1)
             , "exp"
             )  
    funLog = ( log, recip
             , map (\x -> (x,x/3)) [0.1,0.2 .. 2] 
             , "log"
             )

testEquation :: [Test]
testEquation = 
  [ TestCase $ assertBool "Bisection" $ ok (pi/2) (solveBisection 0 (1,2) cos)
  , TestCase $ assertBool "Ridders"   $ ok (pi/2) $ solveRidders   0 (1,2) cos
  , TestCase $ assertBool "Newton"    $ ok (pi/2) $ solveNewton    0 (1,2) cos sin
  ]
  where
    ok exact (Root x) = within 1 exact x
    ok _     _        = False

main :: IO ()
main = do 
  res <- runTestTT $ TestList $ concat [ testDifferentiation
                                       , testIntegration
                                       , testEquation
                                       ]
  print res
