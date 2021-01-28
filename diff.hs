import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E

main :: IO ()
main = do
    filePath:_ <- getArgs
    fileText <- B.readFile filePath
    let bytesLength = B.length fileText
        textLength = (T.length . E.decodeUtf8) fileText
        diff = bytesLength - textLength
    print diff