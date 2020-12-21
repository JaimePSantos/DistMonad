module Main where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import QWalk

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,1/x ) | x <- xs ]
quantumPlot n = stateList n initCondQuantum
main =
    toFile def "example1_big.svg" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red]
    plot (line "am" [quantumPlot 20])
    plot (points "am points" (quantumPlot 20))
