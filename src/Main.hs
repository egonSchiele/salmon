import Common
import Types
import Utils
import Parsers

maybeModifyState x = return ()

hasUnresolved [] = False
hasUnresolved ((Unresolved _):xs) = True
hasUnresolved (x:xs) = hasUnresolved xs

parseRuby :: Ruby -> StateT CodeState IO Ruby
parseRuby (Unresolved line) = do
  case parse (classParser <||> functionParser <||> embeddedParser) "" line of
      Left err -> error (show err)
      Right result -> do
                maybeModifyState result
                parseRuby result

parseRuby (New c params_) = do
  if hasUnresolved params_
    then do
      newParams <- mapM parseRuby params_
      return $ New c newParams
    else return $ New c params_

parseRuby (Embedded xs) = do
    newXs <- mapM parseRuby xs
    return $ Embedded newXs

parseRuby (Function n a b@(Unresolved _)) = do
    newBody <- parseRuby b
    return $ Function n a newBody

parseRuby x = return x

convert :: String -> StateT CodeState IO ()
convert filename = do
    contents <- liftIO $ lines <$> readFile filename
    rubyLines <- forM (map Unresolved contents) parseRuby
    let newContents = toRuby <$> rubyLines

    liftIO $ writeFile ("_" ++ filename) (join "\n" newContents)

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
