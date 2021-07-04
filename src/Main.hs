module Main where

import Data.Maybe
import Types

instance Show Colour where
  show (Colour r g b) = show r ++ ' ' : show g ++ ' ' : show b ++ " "

rainbowPicture :: [[Colour]]
rainbowPicture = [[Colour r g b | r <- range] | g <- range]
  where
    range = [1 .. 255]
    b = 63

createPpm :: [[Colour]] -> Maybe String
createPpm pixels
  | valid_shape = Just . unlines . add_prefix . map show . concat $ pixels
  | otherwise = Nothing
  where
    height = length pixels
    valid_height = height /= 0
    width = length . head $ pixels
    valid_width = all ((== width) . length) pixels
    valid_shape = valid_height && valid_width
    add_prefix arr = ["P3", show width ++ " " ++ show height, "255"] ++ arr

main = do
  putStrLn . fromJust . createPpm $ rainbowPicture
