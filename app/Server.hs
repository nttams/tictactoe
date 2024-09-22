{-# LANGUAGE TypeApplications #-}

module Main where

import System.Random (randomRIO)

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

import qualified Data.Aeson
import qualified Data.ByteString.Lazy as L

import Types
import Matrix

globalWinSize :: Int
globalWinSize = 3

randomPosition :: Int -> IO Position
randomPosition size = do
    row <- randomRIO @Int (0, size)
    col <- randomRIO @Int (0, size)
    return (row, col)

machinePlay :: Matrix -> IO (Matrix, Position)
machinePlay matrix = do
    pos <- randomPosition (getMatrixSize matrix - 1)
    if isPositionEmpty matrix pos then
        return (updateMatrix matrix pos X, pos)
    else machinePlay matrix

application :: Network.Wai.Request -> (Response -> IO b) -> IO b
application request respond = do 
    reqBody <- getRequestBodyChunk request
    let myRequest = Data.Aeson.decode (L.fromStrict reqBody) :: Maybe ClientRequest
    case myRequest of 
        Nothing -> respond $ responseLBS status400 [] L.empty
        Just (ClientRequest playerNewPos matrix) -> do
            putStrLn "Got new request:"
            L.putStr (L.fromStrict reqBody)
            putStrLn ""
            let matrixAfterPlayerMove = updateMatrix matrix playerNewPos O
            if checkWin matrixAfterPlayerMove playerNewPos O globalWinSize then
                respond $ responseLBS status200 [] (Data.Aeson.encode (ServerResponse USER_WON matrixAfterPlayerMove))
            else do
                (matrixAfterMachineMove, machineNewPos) <- machinePlay matrixAfterPlayerMove
                if checkWin matrixAfterMachineMove machineNewPos X globalWinSize then
                    respond $ responseLBS status200 [] (Data.Aeson.encode (ServerResponse MACHINE_WON matrixAfterMachineMove))
                else
                    respond $ responseLBS status200 [] (Data.Aeson.encode (ServerResponse KEEP_PLAYING matrixAfterMachineMove))

main :: IO()
main = run 3000 application
