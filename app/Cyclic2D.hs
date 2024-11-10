{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
module Cyclic2D where
import Cellular
import Control.Comonad
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import qualified Data.Vector as V
import Data.MonoTraversable
import DeriveMonoComonadTH

data Cell = Cell { val :: Int, total :: Int }
newtype Cyclic2D = Cyclic2D (Univ Cell)

$(deriveMonoInstances ''Cyclic2D)

instance CA Cyclic2D where
  stepCell  = Cyclic2D.stepCell
  renderCA = Cyclic2D.renderCA

stepCell :: Cyclic2D -> Cell
stepCell (Cyclic2D s) =
    cell'
    where
        cell = extract s
        cell' = if hasNextNeighbour (getUnivNeighbours s)
           then Cell { val = (val cell + 1) `mod` (total cell), total = total cell}
           else cell
        hasNextNeighbour neighbours = any (\c -> val c == ((val cell) + 1) `mod` (total cell)) neighbours

renderCA :: CADiagramBackend b => Cyclic2D -> QDiagram b V2 (N b) Any
renderCA (Cyclic2D univ) = univToDiagram cellToDiagram univ


hexes :: [Char]
hexes = ['0'..'9'] ++ ['A'..'F']

colors =  map (\c -> sRGB24read ("#" ++ (replicate 6 c))) hexes

cellToDiagram :: CADiagramBackend b => Cell -> QDiagram b V2 (N b) Any
cellToDiagram Cell{val=v, ..} = rect 1 1 # fc (colors !! v)

{-
cellToDiagram Cell{val=0, ..}  = rect 1 1# fc (sRGB24read "#010101")
cellToDiagram Cell{val=1, ..}  = rect 1 1# fc (sRGB24read "#111111")
cellToDiagram Cell{val=2, ..} = rect 1 1 # fc (sRGB24read "#222222")
cellToDiagram Cell{val=3, ..} = rect 1 1 # fc (sRGB24read "#333333")
cellToDiagram Cell{val=4, ..} = rect 1 1 # fc (sRGB24read "#444444")
cellToDiagram Cell{val=5, ..} = rect 1 1 # fc (sRGB24read "#555555")
cellToDiagram Cell{val=6, ..} = rect 1 1 # fc (sRGB24read "#666666")
cellToDiagram Cell{val=7, ..} = rect 1 1 # fc (sRGB24read "#777777")
cellToDiagram Cell{val=8, ..} = rect 1 1 # fc (sRGB24read "#888888")
cellToDiagram Cell{val=9, ..} = rect 1 1 # fc (sRGB24read "#999999")
cellToDiagram Cell{val=10, ..} = rect 1 1 # fc (sRGB24read "#AAAAAA")
cellToDiagram Cell{val=11, ..} = rect 1 1 # fc (sRGB24read "#BBBBBB")
cellToDiagram Cell{val=12, ..} = rect 1 1 # fc (sRGB24read "#CCCCCC")
cellToDiagram Cell{val=13, ..} = rect 1 1 # fc (sRGB24read "#DDDDDD")
cellToDiagram Cell{val=14, ..} = rect 1 1 # fc (sRGB24read "#EEEEEE")
cellToDiagram Cell{val=15, ..} = rect 1 1 # fc (sRGB24read "#EFEFEF")
cellToDiagram Cell{..} = square 1 # fc (sRGB 0.2 (1.0 - 0.2) 0.0)
-}

