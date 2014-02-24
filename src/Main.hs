import Common
import Types
import Utils
import Parsers

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
