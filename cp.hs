import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    copyFilePath:targetFilePath:_ <- getArgs
    contentFile <- TIO.readFile copyFilePath
    TIO.writeFile targetFilePath contentFile