import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    path:_ <- getArgs
    fileContent <- TIO.readFile path
    TIO.writeFile path $ T.toUpper fileContent
