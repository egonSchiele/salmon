import Common
import Types
import Utils
import Parsers

maybeModifyState [] = return ()
maybeModifyState (con@(Constructor n f):xs) = do
    modify $ over classes (con:)
    maybeModifyState xs

maybeModifyState [RunBlock] = modify $ set runBlock True
maybeModifyState [EndRunBlock] = modify $ set runBlock False
maybeModifyState x = return ()

parseLine :: String -> StateT CodeState IO [RubyData]
parseLine line = do
  state <- get
  let parsers_ = (dataParser <||> newParser <||> runBlockParser)
      parsers = if (state ^. runBlock) then (runBlockStatement <||> parsers_) else parsers_
  case parse parsers "" line of
      Left err -> return $ [UnchangedLine line]
      Right result -> do
                maybeModifyState result
                return result

convert :: String -> StateT CodeState IO ()
convert filename = do
    contents <- liftIO $ lines <$> readFile filename
    newContents <- forM contents parseLine

    liftIO $ writeFile ("_" ++ filename) (join "\n" $ map toRuby (concat newContents))

printHelp = do
    putStrLn "salmon adds some extra syntax to Ruby."
    putStrLn "In particular, you can use `data Maybe = Nothing | Just a` now."
    putStrLn "Give it a filename and it will generate _filename.rb."

main = do
    args <- getArgs
    case args of
      ["-h"] -> printHelp
      ["--help"] -> printHelp
      [filename] -> runStateT (convert filename) defaultState >> return ()
      _ -> printHelp
