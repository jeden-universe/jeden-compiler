{-# LANGUAGE
        RankNTypes,
        OverloadedStrings
  #-}

module Main (main) where

import Prelude hiding ( concat, getLine, putStr, putStrLn, readFile )

import Data.ByteString
    ( ByteString, snoc )
import Data.ByteString.Char8
    ( unpack, concat, getLine, putStr, putStrLn, readFile )
import Data.ByteString.Short
    ( ShortByteString )
import Data.ByteString.UTF8
    ( fromString )
import qualified Data.Map as Map

import Text.ByoParser
import Text.ByoParser.Result
import Text.ByoParser.Stream

import Pointless.Lambda
import Pointless.Parse    ( plambda, ptermdef )
import Pointless.Type


type Parser a = forall r. ParserPrim ByteString String () r a

main :: IO ()
main = do
    putStrLn $ concat motd
    repl (Env Map.empty Map.empty)
    return ()

type Module = Map.Map ShortByteString PLambda
type Binds  = Map.Map ShortByteString PLambda

data Env = Env {
    defs  :: Module,
    binds :: Binds
}

updateDefs :: (Module -> Module) -> Env -> Env
updateDefs update env =
    env { defs = update (defs env) }

repl :: Env -> IO Env
repl env = do
    putStr "\x1B[2m>>> \x1B[0m"
    line <- getLine
    case parsePartial command (snoc line 10) () of
        PFail err -> do
            putStrLn $ fromString $ show err
            repl env
        PDone res ->
--            putStrLn $ "Warning: ignored \"" ++ rest
            process res env
        partial ->
            more partial
    where
    more cont = do
        putStr "... "
        line <- getLine
        case feed cont (snoc line 10) of
            PFail err -> do
                putStrLn $ fromString $ show err
                repl env
            PDone res ->
--                putStrLn $ "Warning: ignored \"" ++ rest
                process res env
            partial ->
                more partial

data Command =
    Expr SLambda
  | Def  ShortByteString SLambda
  | Load ByteString
  | Show ShortByteString
  | Quit

process :: Command -> Env -> IO Env
process (Def nam lam) env =
    repl env { binds = Map.insert nam lam (binds env) }

process (Expr mor) env =
    repl env
    -- case runMorphism prog mor (binds,[]) of
    --     Just (binds', []) ->
    --         repl (prog,binds')
    --     Just _ -> do
    --         putStrLn $ "Error: uncaptured output arguments."
    --         repl (prog,binds)
    --     Nothing -> do
    --         putStrLn $ "Fail."
    --         repl (prog,binds)

process (Show name) env =
    case Map.lookup name (binds env) of
        Just v -> do
            putStrLn $ fromString $ render (pPrint v)
            repl env
        Nothing -> do
            putStrLn $ "Error: undefined variable."
            repl env

process (Load name) env = do
    file <- readFile $ unpack name
    case parseEither pmodule file () of
        Left err -> do
            putStrLn $ fromString err
            repl env
        Right newdefs -> do
            repl env
            -- repl (updateDefs (Map.union $ newdefs) env)
            -- TODO process definitions

process Quit env = return env

command :: Parser Command
command =
        ( do
            string "#show"
            psomespace
            name <- pident
            pmanyspace
            token 10
            return $ Show name
        )
    <|> ( do
            string "#load"
            psomespace
            name <- takeWhile1 isAsciiLetter  -- TODO change to filename
            return $ Load name
        )
    <|> ( do
            string "#quit"
            return Quit
        )
    <|> ( do
            (nam,lam) <- termdef
            return $ Def nam lam
        )
    <|> ( do
            lam <- lambda
            return $ Expr lam
        )

motd :: [ByteString]
motd = [
    "Jeden interpreter, pointless core\n",
    "=================================\n"
    ]
