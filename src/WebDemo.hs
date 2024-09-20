{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

main :: IO ()
main = scotty 3000 $
    get "/:word" $ do
        word <- pathParam "word"
        html ("<h1>I " <> word <> " advprpa!</h1>")
