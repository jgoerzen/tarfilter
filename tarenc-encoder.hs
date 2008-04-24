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
import qualified Data.ByteString as BS
import Text.ParserCombinators.Parsec
import System.Environment
import HSH
import System.IO
import Data.Int
import Control.Monad
import System.Posix.IO
import Control.Exception(evaluate)
import Data.IORef
import System.Path
import System.IO.Unsafe(unsafeInterleaveIO)

type InputTarContent = (Int64, FilePath)
type InputTarSize = (FilePath, Maybe Int64)

main :: IO ()
main = brackettmpdir "tarenc-encoder.XXXXXX" $ \tmpdir -> do
    do --updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       (tardatafn, encoder, offsetfn) <- case argv of
                                [x, y, z] -> return (x, y, z)
                                _ -> usage
       
       offsetH <- openFile offsetfn WriteMode
       tarDataH <- openFile tardatafn ReadMode
       tarData <- BSL.readFile tardatafn
       hSetBuffering stdin LineBuffering
       blockData <- getContents
       let sizes = convToSize . parseMinusR $ blockData

       procData tmpdir encoder offsetH tarData sizes
       hClose offsetH

procData :: FilePath -> String -> Handle -> BSL.ByteString -> [InputTarSize] -> IO ()
procData tmpdir encoder offseth = 
    pdworker (tmpdir ++ "/sizefn") encoder offseth 0 

pdworker :: FilePath -> String -> Handle -> Int64 -> BSL.ByteString -> [InputTarSize] -> IO ()
pdworker _ _ _ _ _ [] = return ()
pdworker sizefp encoder offseth bytesWritten inp (thisSize:xs) =
    case thisSize of
      (fp, Nothing) -> -- Last entry
          do newlen <- writeEncoded inp
             write newlen fp
      (fp, Just sz) -> -- Regular entry
          do let fullsize = sz * 512
             let (thiswrite, remainder) = BSL.splitAt fullsize inp
             newlen <- writeEncoded thiswrite
             write newlen fp
             pdworker sizefp encoder offseth (bytesWritten + newlen) remainder xs
    where write :: Int64 -> FilePath -> IO ()
          write l fp =
              hPutStrLn offseth $ show bytesWritten ++ "\t" ++ show l ++ "\t" ++
                        fp
          writeEncoded x =
              do runIO $ echoBS x -|- encoder -|- countBytes sizefp
                 -- BSL.putStr x
                 countStr <- readFile sizefp
                 -- let countStr = "0"
                 -- hPutStrLn stderr $ "writeEncoded: got " ++ show countStr
                 return (read countStr)

echoBytes :: Int64 -> BSL.ByteString -> BSL.ByteString
echoBytes = BSL.take

countBytes :: FilePath -> BSL.ByteString -> IO BSL.ByteString
countBytes fp inp = 
    do let chunks = BSL.toChunks inp
       ref <- ((newIORef 0) :: IO (IORef Int64))
       resultChunks <- procChunks ref chunks
       return (BSL.fromChunks resultChunks)

    where procChunks ref chunks = unsafeInterleaveIO $
            case chunks of
              [] -> do sz <- readIORef ref
                       writeFile fp (show sz)
                       return [BS.empty]
              (x:xs) -> do modifyIORef ref (\sz -> sz + (fromIntegral . BS.length $ x))
                           remainder <- procChunks ref xs
                           return (x : remainder)
                           
usage :: IO a
usage =
    do putStrLn "Usage:\n"
       putStrLn "tarenc-encoder tardatafifopath encoder outputoffsetfilename"
       putStrLn "input from stdin, output tar file is written to stdout"
       putStrLn "use /dev/null for outputoffsetfilename if you don't want offset info"
       fail "Usage error"
       
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
