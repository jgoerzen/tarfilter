{- 
Copyright (C) 2008 John Goerzen <jgoerzen@complete.org>

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
-}

{- |
   Module     : Main
   Copyright  : Copyright (C) 2008 John Goerzen
   License    : GNU GPL, version 3 or above; see COPYRIGHT for details

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

import qualified Data.ByteString.Lazy as BSL
import Text.ParserCombinators.Parsec
import System.Environment
import HSH
import System.IO
import Data.Int
import Data.IORef

type InputTarContent = (Int64, FilePath)
type InputTarSize = (FilePath, Maybe Int64)

main :: IO ()
main = 
    do --updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       (encoder, offsetfn) <- case argv of
                                [x, y] -> return (x, y)
                                _ -> usage
       
       inpData <- BSL.getContents

       inpcontent_str <- scanInput inpData
       let sizes = convToSize . parseMinusR $ inpcontent_str

       procData encoder inpData sizes
       hPrint stderr (BSL.length inpData)

procData :: String -> BSL.ByteString -> [InputTarSize] -> IO ()
procData encoder = pdworker encoder 0 
pdworker _ _ _ [] = return ()
pdworker encoder bytesWritten inp (thisSize:xs) =
    case thisSize of
      (fp, Nothing) -> -- Last entry
          do newlen <- writeEncoded inp
             write newlen fp
      (fp, Just sz) -> -- Regular entry
          do let fullsize = sz * 512
             let (thiswrite, remainder) = BSL.splitAt fullsize inp
             newlen <- writeEncoded thiswrite
             write newlen fp
             pdworker encoder (bytesWritten + newlen) remainder xs
    where write l fp =
              hPutStrLn stderr $ show bytesWritten ++ "\t" ++ show l ++ "\t" ++
                        fp
          writeEncoded x =
              do ref <- newIORef 0
                 runIO $ echoBS x -|- encoder -|- countBytes ref
                 readIORef ref

countBytes :: IORef Int64 -> BSL.ByteString -> IO BSL.ByteString
countBytes mv inp = 
    do byteCount <- worker 0 inp
       writeIORef mv byteCount
       return BSL.empty
    where worker :: Int64 -> BSL.ByteString -> IO Int64
          worker bytesWritten toWrite
              | BSL.null toWrite = return bytesWritten
              | otherwise =
                  do let (thisChunk, nextChunk) = BSL.splitAt 8192 toWrite
                     BSL.putStr thisChunk
                     worker (bytesWritten + BSL.length thisChunk) nextChunk
           
usage =
    do putStrLn "Usage:\n"
       putStrLn "tarenc encoder outputoffsetfilename"
       putStrLn "input from stdin, output tar file is written to stdout"
       putStrLn "use /dev/null for outputoffsetfilename if you don't want offset info"
       fail "Usage error"
       
scanInput :: BSL.ByteString -> IO String
scanInput inp =
    run $ echoBS inp -|- ("tar", ["-Rtf", "-"])

parseMinusR :: String -> [InputTarContent]
parseMinusR = map procLine . lines
    where procLine l = case parse entry ("Line: " ++ show l) l of
                         Left x -> error (show x)
                         Right y -> y
          entry = do string "block "
                     bn <- many1 digit
                     string ": "
                     fn <- many1 (noneOf "\n\r")
                     eof
                     return (read bn, fn)
          
convToSize :: [InputTarContent] -> [InputTarSize]
convToSize (i1:i2:xs) =
    (snd i1, Just (fst i2 - fst i1)) : convToSize (i2:xs)
convToSize [i1] = 
    [(snd i1, Nothing)]
convToSize [] = []
