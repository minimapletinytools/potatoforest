

module Main where

import           Potato.Forest.Methods
import           Reflex.Dom
import           Relude

main :: IO ()
main = mainWidget $ el "div" $ text $ helloPotato
