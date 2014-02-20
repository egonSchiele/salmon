import Common
import Types
import Utils
import Parsers

-- if you don't use try, and the first parser consumes some input,
-- parser #2 doesn't use that input
(<||>) p1 p2 = try(p1) <|> p2

maybeModifyState [] = return ()
maybeModifyState (con@(Constructor n f):xs) = do
    modify $ over classes (con:)
    maybeModifyState xs
maybeModifyState x = return ()

parseLine :: String -> StateT CodeState IO [RubyData]
parseLine line = do
  case parse (dataParser <||> newParser) "" line of
      Left err -> return $ [UnchangedLine (show err ++ line)]
      Right result -> do
                maybeModifyState result
                return result

convert :: String -> StateT CodeState IO ()
convert filename = do
    contents <- liftIO $ lines <$> readFile filename
    newContents <- forM contents parseLine

    liftIO $ writeFile ("_" ++ filename) (join "\n" $ map toRuby (concat newContents))

main = do
    args <- getArgs
    case args of
      [] -> runStateT (convert "test.rb") defaultState
      [filename] -> runStateT (convert filename) defaultState
