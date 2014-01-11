{-# LANGUAGE LambdaCase #-}
-- | lookup your public IP address

module Main where

import Control.Applicative ((<$>))
import Network.Stun
import Network.Socket
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    (host) <- getArgs >>= \case
        [] -> return ("stunserver.org")
        [h] -> return h
        _ -> do
            hPutStrLn stderr "Error: Too may arguments"
            hPutStrLn stderr "usage: stunlookup [stunserver]"
            exitFailure
    getAddrInfo Nothing (Just host) (Just "stun") >>= \case
        [] -> do
            hPutStrLn stderr $ "Error: Could not find server " ++ host
            exitFailure
        (h:_) -> do
            findMappedAddress (addrAddress h) 0 [] >>= \case
                Left e -> do
                    hPutStrLn stderr $ "Error: stun returned error " ++ show e
                    exitFailure
                Right (r, _) -> case r of
                    SockAddrInet{} -> putStrLn (takeWhile (/= ':') $ show r)
                    SockAddrInet6{} -> putStrLn $
                                         tail (takeWhile (/= ']') $ show r)
                    _ -> print r
