import Numeric.Tools.Integration
import Numeric.Tools.Differentiation
import Criterion.Main

-- Random function from "Numerical recipes"
blamg :: Double -> Double
blamg x = x^4 * log(x + sqrt (x*x + 1))

main = defaultMain 
       [ bench "Trapeze[exp]"   $ nf (quadRes . quadTrapezoid  defQuad (0,2)) exp
       , bench "Trapeze[blamg]" $ nf (quadRes . quadTrapezoid  defQuad (0,2)) exp       
       , bench "Simpson[exp]"   $ nf (quadRes . quadSimpson    defQuad (0,2)) exp
       , bench "Simpson[blamg]" $ nf (quadRes . quadSimpson    defQuad (0,2)) exp       
       , bench "Romberg[exp]"   $ nf (quadRes . quadRomberg    defQuad (0,2)) exp
       , bench "Romberg[blamg]" $ nf (quadRes . quadRomberg    defQuad (0,2)) exp

       , bench "Richardson[exp]"   $ nf (diffRes . diffRichardson exp   1) 1
       , bench "Richardson[blamg]" $ nf (diffRes . diffRichardson blamg 1) 1
       ]
