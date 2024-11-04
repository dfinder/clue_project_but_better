module Main (main) where

import Lib
import Brick 
import Microlens
ui::Widget()
ui = str "Hello, World!"
main :: IO()
main = simpleMain ui

-- First: select client or host. Host will display IP and port
-- Client will get a box to fill in IP, port