import Common
import Types
import Utils
import Parsers

convert :: String -> StateT CodeState IO ()
convert filename = do
    contents <- liftIO $ lines <$> readFile filename
    newContents <- forM contents $ \line -> do
                     case parse dataParser "" line of
                       Left _ -> return line
                       Right cls -> do
                         modify $ over classes (++cls)
                         return $ join "\n" $ map toRuby cls

    liftIO $ writeFile ("_" ++ filename) (join "\n" newContents)

main = do
    args <- getArgs
    case args of
      [] -> runStateT (convert "test.rb") defaultState
      [filename] -> runStateT (convert filename) defaultState
