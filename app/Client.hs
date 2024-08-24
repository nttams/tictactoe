module Main where

import qualified Network.HTTP.Simple
import qualified Data.Aeson
import qualified Data.Map
import qualified Data.ByteString.Lazy as L

import Control.Exception (try, SomeException)

import Types
import Matrix

sendRequest :: ClientRequest -> IO (Maybe ServerResponse)
sendRequest req = do
    rawRequest <- Network.HTTP.Simple.parseRequest "POST http://localhost:3000"
    let request = Network.HTTP.Simple.setRequestBodyLBS (Data.Aeson.encode req) rawRequest

    response <- try (Network.HTTP.Simple.httpLBS request) ::  IO (Either SomeException (Network.HTTP.Simple.Response L.ByteString))

    case response of
        Left err -> do
            putStrLn $ "Error: " ++ show err
            return Nothing
        Right res -> do
            let responseBody = Network.HTTP.Simple.getResponseBody res
            return (Data.Aeson.decode responseBody :: Maybe ServerResponse)

clientPlay :: Matrix  -> IO()
clientPlay matrix = do
    putStrLn "Your turn, your mark is O"
    content <- getLine 
    let position = parseInput content :: Maybe (Int, Int)
    case position of 
        Nothing -> do
            putStrLn "Cannot parsed input. Enter again"
            clientPlay matrix
        Just pos ->
            if not $ isValidInput pos then do
                putStrLn "Invalid input. Enter again"
                clientPlay matrix
            else if not $ isPositionEmpty matrix pos then do 
                putStrLn "Position is taken. Enter again"
                clientPlay matrix
            else do
                response <- sendRequest $ ClientRequest pos matrix
                case response of 
                    Nothing -> do
                        putStrLn "Unexpected error, try again"
                        printMatrix matrix
                        clientPlay matrix
                    Just (ServerResponse state newMatrix) -> do
                        printMatrix newMatrix
                        if state == MACHINE_WON then 
                            putStrLn "Machine won"
                        else if state == USER_WON then 
                            putStrLn "Player won"
                        else if state == KEEP_PLAYING then 
                            clientPlay newMatrix
                        else 
                            putStrLn "Unrecognized message"

main :: IO ()
main = clientPlay Data.Map.empty