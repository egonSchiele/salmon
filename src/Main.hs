import Common
import Types
import Utils
import Parsers

maybeModifyState o@(Operator op name_) = modify $ over operators (o:)
maybeModifyState x = return ()

hasUnresolved [] = False
hasUnresolved ((Unresolved _):xs) = True
hasUnresolved (x:xs) = hasUnresolved xs

isUnresolved (Unresolved _) = True
isUnresolved _ = False

-- | This is what computes the AST. The main method is the one that parses
-- `Unresolved` objects. There are others which will take an existing Ruby
-- object, and check if any parts of it are unresolved. If so, it feeds
-- them back into `parseRuby`.
parseRuby :: Ruby -> StateT CodeState IO Ruby
parseRuby (Unresolved line) = do
  state <- get
  let ops = state ^. operators
  case parse (classParser <||> functionParser <||> operatorParser <||> (embeddedParser ops) <||> idParser) "" line of
      Left err -> error (show err)
      Right result -> do
                maybeModifyState result
                parseRuby result

-- debugging
-- parseRuby i@(Identifier line) = do
--     liftIO $ print i
--     return i

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

parseRuby (InfixCall left name_ right) = do
    newLeft <- parseRuby left
    newRight <- parseRuby right
    return $ InfixCall newLeft name_ newRight

parseRuby x = return x

convert :: String -> StateT CodeState IO ()
convert filename = do
    contents <- liftIO $ lines <$> readFile filename
    rubyLines <- forM (map Unresolved contents) parseRuby
    let newContents = toRuby <$> (concatRuby rubyLines)
        newFilename = dropEnd 4 filename ++ ".rb"
  
    liftIO $ writeFile (newFilename) (join "\n" newContents)

printHelp = do
    putStrLn "salmon adds some extra syntax to Ruby."
    putStrLn "In particular, you can use `data Maybe = Nothing | Just a` now."
    putStrLn "Give it a filename and it will generate _filename.rb."

main = do
    args <- getArgs
    case args of
      ["-h"] -> printHelp
      ["--help"] -> printHelp
      [filename] -> do
        if (takeEnd 4 filename /= ".slm")
          then putStrLn "Please give me a file with a .slm extension."
          else runStateT (convert filename) defaultState >> return ()
      _ -> printHelp
