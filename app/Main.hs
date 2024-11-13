{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Cellular
import qualified Data.Vector as V
import Diagrams.Core.Compile
import Diagrams.Core.Types
import Diagrams.Backend.CmdLine
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Rasterific
import Diagrams.TwoD
import Data.Monoid (Any)

import qualified GameOfLife
import qualified Seeds
import qualified BriansBrain
import qualified Cyclic1D
import qualified Cyclic2D
import qualified Heat1D
import qualified Rule
import System.Random


import Foreign.C
import Foreign
import Diagrams.Prelude
import Data.Colour.SRGB (sRGB)  -- Import sRGB to construct RGB color


-- Define a foreign export function named 'generate'
foreign export ccall test :: CInt -> CInt -> CString -> IO CInt
foreign export ccall generate :: CString -> CInt -> CInt -> CString -> IO CInt

test :: CInt -> CInt -> CString -> IO CInt
test h w o = do
    -- Convert the CString to a Haskell String
    outputFile <- peekCString o

    -- Convert CInt to Int for height and width
    let height = fromIntegral h :: Int
        width = fromIntegral w :: Int

    -- Debugging: print the parameters
    putStrLn $ "Generating diagram with height: " ++ show height ++ ", width: " ++ show width
    putStrLn $ "Output file: " ++ outputFile

    -- Define a simple diagram for demonstration
    let diagram :: Diagram B
        diagram = rect (fromIntegral width) (fromIntegral height)
                    # fc (sRGB 0 0 1)  -- Example: a blue rectangle (RGB(0, 0, 1))

    -- Set up the backend rendering options
    let renderOpts :: FilePath -> DiagramOpts
        renderOpts outpath = DiagramOpts { _width = Just width, _height = Just height, _output = outpath }

    -- Create the SizeSpec (width and height) for rendering, convert to Double
    let sizeSpec = mkSizeSpec2D (Just $ fromIntegral width) (Just $ fromIntegral height)

    -- Render the diagram using the Rasterific backend
    putStrLn "Rendering the diagram..."
    renderRasterific outputFile sizeSpec diagram

    -- Return 0 to indicate success
    return 0

generate :: CString -> CInt -> CInt -> CString -> IO CInt
generate c h w o = do

    caType <- peekCString c
    outputFile <- peekCString o
    let height = fromIntegral h :: Int
    let width = fromIntegral w :: Int

    -- Debugging: print the parameters
    putStrLn $ "Generating diagram with height: " ++ show height ++ ", width: " ++ show width
    putStrLn $ "Output file: " ++ outputFile

    let renderOpts :: FilePath -> (DiagramOpts, GifOpts) --MainOpts [(QDiagram Rasterific V2 n Any, Int)]
        renderOpts outpath = let
                  diagramOpts = DiagramOpts { _width = Just width, _height = Just height, _output = outputFile }
                  gifOpts = GifOpts {_dither = False, _noLooping = False, _loopRepeat = Nothing}
                 in (diagramOpts, gifOpts)

    let caGifMain :: CA ca => FilePath -> IO ca -> Steps -> IO ()
        caGifMain outpath iostart nsteps = do
          start <- iostart
          mainRender ((renderOpts outpath) :: MainOpts [(QDiagram Rasterific V2 n Any, Int)]) (mkCAGif start nsteps)

    let caImageMain :: CA ca => FilePath -> IO ca -> Steps -> IO ()
        caImageMain outpath iostart nsteps = do
            start <- iostart
            let diagram = mkCAImage start nsteps
            let sizeSpec = dims2D (fromIntegral width) (fromIntegral height)
            renderRasterific outpath sizeSpec diagram

    case caType of
        "gol" -> do
            putStrLn "Generating Game of Life GIF..."
            generateGolGif caGifMain outputFile
            putStrLn "Finished Generating Game of Life GIF..."
            return 0
        "bb" -> do
            putStrLn "Generating Brians Brain GIF..."
            generateBriansBrainGif caGifMain outputFile
            putStrLn "Finished Generating Brians Brain GIF..."
            return 0
        "c1d" -> do
            putStrLn "Generating Cyclic 1D GIF..."
            generateCyclic1dGif caGifMain outputFile
            putStrLn "Finished Generating Cyclic 1..."
            return 0  -- Successful execution
        _ -> do
            putStrLn $ "Unknown type: " ++ caType
            return 1

    return 0

generateGolGif :: (FilePath -> IO GameOfLife.GameOfLife -> Steps -> IO ()) -> FilePath -> IO ()
generateGolGif caGifMain outputFile = do
    let golDim = 20

    let golGenerator :: IO GameOfLife.Cell
        golGenerator = do
          val <- getStdRandom (randomR (0, 1)) :: IO Int
          return $ if val == 0 then GameOfLife.Off else GameOfLife.On

    let golStartGrid :: IO GameOfLife.GameOfLife
        golStartGrid = do
          univ <- makeUnivM golDim (const . const $ golGenerator)
          return $ GameOfLife.GameOfLife univ

    -- Call caGifMain passed as argument
    caGifMain outputFile golStartGrid 10


generateBriansBrainGif :: (FilePath -> IO BriansBrain.BriansBrain -> Steps -> IO ()) -> FilePath -> IO ()
generateBriansBrainGif caGifMain outputFile = do
    let briansDim = 20

    let briansGenerator :: IO BriansBrain.Cell
        briansGenerator = do
          newStdGen
          val <- getStdRandom (randomR (0, 3)) :: IO Int
          let cell = case val of
                      0 -> BriansBrain.On
                      1 -> BriansBrain.Off
                      2 -> BriansBrain.Off
                      3 -> BriansBrain.Off

          return cell

    let briansStartGrid :: IO (BriansBrain.BriansBrain)
        briansStartGrid = do
          univ <- makeUnivM briansDim (const . const $ briansGenerator)
          return $ BriansBrain.BriansBrain univ

    caGifMain outputFile briansStartGrid 10

generateCyclic1dGif :: (FilePath -> IO Cyclic1D.Cyclic1D -> Steps -> IO ()) -> FilePath -> IO ()
generateCyclic1dGif caGifMain outputFile = do

    let cyclic1dDim = 20
    let cyclic1dTypes = 4

    let cyclic1dGenerator :: IO Cyclic1D.Cell
        cyclic1dGenerator = do
          newStdGen
          val <- getStdRandom (randomR (0, cyclic1dTypes)) :: IO Int
          return $ Cyclic1D.Cell val cyclic1dTypes

    let cyclic1dStartGrid :: IO (Cyclic1D.Cyclic1D)
        cyclic1dStartGrid = do
          rz <- makeRingZipperM cyclic1dDim (const $ cyclic1dGenerator)
          return $ Cyclic1D.Cyclic1D rz

    caGifMain outputFile cyclic1dStartGrid 10
