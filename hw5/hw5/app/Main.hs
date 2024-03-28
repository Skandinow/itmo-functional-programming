module Main (main) where

import           Control.Monad.IO.Class        (liftIO)
import           HW5.Base                      ()
import           HW5.Evaluator
import           HW5.Parser
import           HW5.Pretty
import           Prettyprinter.Render.Terminal (putDoc)
import           System.Console.Haskeline
import           Text.Megaparsec               (errorBundlePretty)


main :: IO ()
main = runInputT defaultSettings replLoop

replLoop :: InputT IO ()
replLoop = do
    minput <- getInputLine "hi> "
    case minput of
        Nothing -> return ()
        Just "q" -> return ()
        Just input -> do
            let parsed = parse input
            case parsed of
              Left err -> liftIO $ putStrLn $ errorBundlePretty err
              Right expr -> do
                result <- eval expr
                case result of
                  Left err -> do
                    liftIO $ putDoc $ prettyError err
                    liftIO $ putStrLn ""
                  Right value -> do
                    liftIO $ putDoc $ prettyValue value
                    liftIO $ putStrLn ""
            replLoop
