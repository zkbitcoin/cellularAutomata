{-# LANGUAGE TemplateHaskell #-}

module BriansBrain where

import Cellular
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import MaterialColors
import qualified Data.Vector as V


data Cell = On | Off | Dying deriving(Eq)

type Grid = Univ Cell

liveNeighbourCount :: Grid -> Int
liveNeighbourCount grid = V.sum $ fmap (\c -> if c == On then 1 else 0) (getUnivNeighbours grid)

stepCell :: Grid -> Cell
stepCell grid =
    cell'
    where
        cell' = if cell == Off && numNeighbours == 2 then On
                else if cell == On then Dying
                else Off
        cell = extract grid
        numNeighbours = liveNeighbourCount $ grid

renderUniv :: Grid -> Diagram B
renderUniv (Univ univ) = gridCat $ V.toList $ fmap cellToDiagram $ join (fmap mergeRingZipper (mergeRingZipper univ))


cellToDiagram :: Cell -> Diagram B
cellToDiagram On = square 1 # fc blue
cellToDiagram Dying = square 1 # fc gray
cellToDiagram Off = square 1 # fc black


--briansBrainCA = Cellular.CellularAutomata {
--    Cellular.stepCell = BriansBrain.stepCell,
--    Cellular.renderUniv = BriansBrain.renderUniv
--}