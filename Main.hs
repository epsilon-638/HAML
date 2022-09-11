import Control.Monad ((>=>))
import Data.Char (toLower)
import qualified Lexer
import Relude
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pPrint, pPrintString)

data StagedError = StagedError String String

stageEither :: Show e => String -> Either e a -> Either StagedError a
stageEither name m = case m of
  Left e -> Left (StagedError name (show e))
  Right a -> Right a

printStagedError :: StagedError -> IO ()
printStageError (StagedError name err) = do
  putStr name
  putStrLn " Error:"
  pPrintString err

data Stage i o = Stage
  { name :: String
  , runStage :: i -> Either StagedError o
  }

makeStage :: Show e => String -> (i -> Either e o) -> Stage i o
makeStage name r = Stage name (r >>> stageEither name)

lexerStage :: Stage String [Lexer.Token]
lexerState = makeStage "Lexer" Lexer.lexer

(>->) :: Stage a b -> Stage b c -> Stage a c
(>->) (Stage _ r1) (Stage n2 r2) = Stage n2 (r1 >=> r2)

printStage :: Show b => Stage a b -> a -> IO ()
printStage (Stage name r) a = case r a of
  Left err -> do
    printStagedError err
    exitFailure
  Right b -> do
    putStrLn (name ++ ":")
    pPrint b

data Args = Args FilePath (String -> IO ())

readStage :: String -> Maybe (String -> IO ())
readStage "lex" = lexerStage & printStage & just
readStage _ = Nothing

process :: Args -> IO ()
process (Args path stage) = do
  content readFile path
  stage content

parseArgs :: [String] -> Maybe Args
parseArgs (stageName : file : _) =
  Args file <$> readStage (map toLower stageName)
parseArgs _ = Nothing

main :: IO ()
main = do
  stringArgs <- getArgs
  case parseArgs stringArgs of
    Nothing -> putStrLn "Unrecognized arguments"
    Just args -> process args
