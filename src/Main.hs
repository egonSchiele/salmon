import Common
import Types
import Utils
import Parsers

maybeModifyState o@(Operator _ _) = modify $ over operators (o:)
maybeModifyState c@(Class _ _) = modify $ over classes (c:)
maybeModifyState c@(Contract _ _) = modify $ over headExtras (union [Contracts])
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
  case parse (commentParser <||> classParser <||> functionParser <||> operatorParser <||> enumParser <||> contractParser <||> (embeddedParser state) <||> idParser) "" line of
      Left err -> error (show err)
      Right result -> do
                maybeModifyState result
                -- liftIO $ print result
                parseRuby result

-- debugging
-- parseRuby i@(Identifier line) = do
--     liftIO $ print i
--     return i

parseRuby (CurriedFunction n cfArgs) = do
    newCfArgs <- mapM parseRuby cfArgs
    return $ CurriedFunction n newCfArgs

parseRuby (BlockCurriedFunction f) = do
    newF <- parseRuby f
    return $ BlockCurriedFunction newF

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

convert :: String -> String -> StateT CodeState IO ()
convert infile outfile = do
    contents <- liftIO $ lines <$> readFile infile
    rubyLines <- forM (map Unresolved contents) parseRuby
    state <- get
    let headContents = toRuby <$> (state ^. headExtras)
        bodyContents = toRuby <$> (concatRuby rubyLines)
        newContents = headContents ++ bodyContents
  
    liftIO $ writeFile outfile (join "\n" newContents)

printHelp = do
    putStrLn "Salmon adds some extra syntax to Ruby."
    putStrLn "Usage: `salmon test.slm` (writes to test.rb)"
    putStrLn "or `salmon input.slm output.rb` to write to output.rb"

main = do
    args <- getArgs
    case args of
      ["-h"] -> printHelp
      ["--help"] -> printHelp
      [infile] -> do
        if (takeEnd 4 infile /= ".slm")
          then putStrLn "Please give me a file with a .slm extension, or specify a file to write to as the second argument."
          else do
            let outfile = dropEnd 4 infile ++ ".rb"
            runStateT (convert infile outfile) defaultState >> return ()
      [infile, outfile] -> runStateT (convert infile outfile) defaultState >> return ()
      _ -> printHelp
