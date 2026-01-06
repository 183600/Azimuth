import System.Process
import System.IO
import Control.Monad
import System.Directory (renameFile)

main = do
  content <- readFile "test/Spec.hs"
  let lines' = lines content
      fixedLines = map fixIndent lines'
  writeFile "test/Spec_fixed.hs" $ unlines fixedLines
  renameFile "test/Spec_fixed.hs" "test/Spec.hs"

fixIndent :: String -> String
fixIndent line
  | "describe " `isPrefixOf` line = "    " ++ line
  | "it " `isPrefixOf` line = "      " ++ line
  | "  describe " `isPrefixOf` line = "  " ++ drop 2 line
  | "    it " `isPrefixOf` line = "    " ++ drop 4 line
  | "      let " `isPrefixOf` line = "        " ++ drop 6 line
  | "      serviceName " `isPrefixOf` line = "        " ++ drop 6 line
  | "      serviceVersion " `isPrefixOf` line = "        " ++ drop 6 line
  | "      enableMetrics " `isPrefixOf` line = "        " ++ drop 6 line
  | "      enableTracing " `isPrefixOf` line = "        " ++ drop 6 line
  | "      enableLogging " `isPrefixOf` line = "        " ++ drop 6 line
  | otherwise = line

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys