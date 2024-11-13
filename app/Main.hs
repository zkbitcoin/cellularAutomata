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
            generateCyclic1DGif caGifMain outputFile
            putStrLn "Finished Generating Cyclic 1D GIF..."
            return 0
        "c2d" -> do
            putStrLn "Generating Cyclic 2D GIF..."
            generateCyclic2DGif caGifMain outputFile
            putStrLn "Finished Generating Cyclic 2D GIF..."
            return 0
        "heat1d" -> do
            putStrLn "Generating Heat 1D GIF..."
            generateHeat1DGif caGifMain outputFile
            putStrLn "Finished Generating Heat 1D GIF..."
            return 0
        -- New rule cases
        "rule184AtCenterMain" -> do
            putStrLn "Generating rule184AtCenterMain JPG..."
            generateRuleImage caImageMain outputFile "rule184AtCenterMain"
            putStrLn "Finished Generating rule184AtCenterMain JPG..."
            return 0
        "rule184NotAtCenterMain" -> do
            putStrLn "Generating rule184NotAtCenterMain JPG..."
            generateRuleImage caImageMain outputFile "rule184NotAtCenterMain"
            putStrLn "Finished Generating rule184NotAtCenterMain JPG..."
            return 0
        "rule184BiasPoint2" -> do
            putStrLn "Generating rule184BiasPoint2 JPG..."
            generateRuleImage caImageMain outputFile "rule184BiasPoint2"
            putStrLn "Finished Generating rule184BiasPoint2 JPGF..."
            return 0
        "rule184BiasPoint8" -> do
            putStrLn "Generating rule184BiasPoint8 JPG..."
            generateRuleImage caImageMain outputFile "rule184BiasPoint8"
            putStrLn "Finished Generating rule184BiasPoint8 JPG..."
            return 0
        "rule90FromCenter" -> do
            putStrLn "Generating rule90FromCenter JPG..."
            generateRuleImage caImageMain outputFile "rule90FromCenter"
            putStrLn "Finished Generating rule90FromCenter JPG..."
            return 0
        "rule90Random" -> do
            putStrLn "Generating rule90Random JPG..."
            generateRuleImage caImageMain outputFile "rule90Random"
            putStrLn "Finished Generating rule90Random JPG..."
            return 0
        -- Default case for unknown caType
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

generateCyclic1DGif :: (FilePath -> IO Cyclic1D.Cyclic1D -> Steps -> IO ()) -> FilePath -> IO ()
generateCyclic1DGif caGifMain outputFile = do

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


generateCyclic2DGif :: (FilePath -> IO Cyclic2D.Cyclic2D -> Steps -> IO ()) -> FilePath -> IO ()
generateCyclic2DGif caGifMain outputFile = do

    let cyclic2dDim = 30
    let cyclic2DTypes = 15

    let cyclic2DGenerator :: IO Cyclic2D.Cell
        cyclic2DGenerator = do
          val <- getStdRandom (randomR (0, cyclic2DTypes - 1))
          return $ Cyclic2D.Cell {
            Cyclic2D.total=cyclic2DTypes,
            Cyclic2D.val=val
          }

    let cyclic2DStartGrid :: IO (Cyclic2D.Cyclic2D)
        cyclic2DStartGrid = do
          univ <-  makeUnivM cyclic2dDim  (const . const $ cyclic2DGenerator)
          return $ Cyclic2D.Cyclic2D univ

    caGifMain outputFile cyclic2DStartGrid 10


generateHeat1DGif :: (FilePath -> IO Heat1D.Heat1D -> Steps -> IO ()) -> FilePath -> IO ()
generateHeat1DGif caGifMain outputFile = do

    let heat1dDim = 100

    {-
    heat1dGenerator :: IO Heat1D.Cell
    heat1dGenerator = do
      newStdGen
      val <- getStdRandom (randomR (0, cyclic1dTypes)) :: IO Float
      return $ Heat1D.Cell val
    -}

    let clampHeat :: Float -> Float
        clampHeat x = min (max x 0) 1

    let heatfn :: Int -> Float
        heatfn x = normx
              where
                normx = (fromIntegral x) / (fromIntegral heat1dDim)

    let heat1dStartGrid :: IO (Heat1D.Heat1D)
        heat1dStartGrid = do
          let rz = makeRingZipper heat1dDim (Heat1D.Cell . clampHeat . heatfn)
          return $ Heat1D.Heat1D rz

    caGifMain outputFile heat1dStartGrid 100

generateRuleImage :: (FilePath -> IO Rule.Rule -> Steps -> IO ()) -> FilePath -> String -> IO ()
generateRuleImage caImageMain outputFile ruleType = do

    let ruledim = 100  -- Grid dimension

    -- Generates a random boolean value
    let randbool :: IO Bool
        randbool = randomIO

    -- Initialize grid with rule index at the center
    let ruleStartGridAtCenter :: Int -> IO Rule.Rule
        ruleStartGridAtCenter ruleix = do
            let rz = makeRingZipper ruledim (\i -> Rule.Cell ruleix (i * 2 == ruledim))
            return $ Rule.Rule rz

    -- Initialize grid with rule index not at the center
    let ruleStartGridNotAtCenter :: Int -> IO Rule.Rule
        ruleStartGridNotAtCenter ruleix = do
            let rz = makeRingZipper ruledim (\i -> Rule.Cell ruleix (i * 2 /= ruledim))
            return $ Rule.Rule rz

    -- Initialize grid with a random boolean value
    let ruleRandomInit :: Int -> IO Rule.Rule
        ruleRandomInit ruleix = do
            rz <- makeRingZipperM ruledim (\i -> Rule.Cell <$> pure ruleix <*> randbool)
            return $ Rule.Rule rz

    -- Generate a random boolean with a bias towards 'b'
    let randBoolBiased :: Float -> IO Bool
        randBoolBiased b = do
            p <- randomIO
            return $ p <= b

    -- Initialize grid with a random boolean, biased by a given ratio
    let ruleRandomInitRatio :: Float -> Int -> IO Rule.Rule
        ruleRandomInitRatio b ruleix = do
            rz <- makeRingZipperM ruledim (\i -> Rule.Cell <$> pure ruleix <*> randBoolBiased b)
            return $ Rule.Rule rz

    case ruleType of
        "rule184AtCenterMain" -> do
            caImageMain outputFile (ruleStartGridAtCenter 184) 10
        "rule184NotAtCenterMain" -> do
            caImageMain outputFile (ruleStartGridNotAtCenter 184) 10
        "rule184BiasPoint2" -> do
            caImageMain outputFile (ruleRandomInitRatio 0.2 184) 10
        "rule184BiasPoint8" -> do
            caImageMain outputFile (ruleRandomInitRatio 0.8 184) 10
        "rule90FromCenter" -> do
            caImageMain outputFile (ruleStartGridAtCenter 90) 20
        "rule90Random" -> do
            caImageMain outputFile (ruleRandomInitRatio 0.2 90) 20
        -- Default case if no match found
        _ -> do
            putStrLn "Invalid ruleType. Please provide a valid ruleType."
            return ()



