module Main where

import Lib
import qualified Data.Text
import Prelude (IO, putStrLn, putStr, getLine )
import Control.Monad (when)
import NriPrelude

write :: Text -> IO ()
write string =
  putStr (Data.Text.unpack string)

writeLine :: Text -> IO ()
writeLine string =
  putStrLn (Data.Text.unpack string)

read :: IO Text
read =
  getLine
    |> fmap Data.Text.pack

main :: IO ()
main = do

    {- sequences and 'mapM' -}


    {- using when -}
    writeLine "choose word to reverse: "
    line <- read
    when (not <| Text.isEmpty line) <| do
        writeLine <| Text.reverse line
        main

    {- reverse a line, then reprompt -}
    -- writeLine "choose word to reverse: "
    -- line <- read
    -- if Text.isEmpty line
    --     then return ()
    --     else do
    --         writeLine <| Text.reverse line
    --         main

    {- simple IO stuff -}
    -- thrownAway <- writeLine "Hello, what's your name?"
    -- name <- read
    -- let bigName = Text.toUpper name
    -- writeLine ("Hey " ++ bigName ++ ", you rock!")

