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
import Data.String.Utils(strip)

type InputTarContent = (Int64, FilePath)
type InputTarSize = (FilePath, Maybe Int64)

main :: IO ()
main = brackettmpdir "tarenc-encoder.XXXXXX" $ \tmpdir -> do
    do --updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       (rawdatafn, encoder, offsetfn) <- case argv of
                                [x, y, z] -> return (x, y, z)
                                _ -> usage
       
       offsetH <- openFile offsetfn WriteMode
       rawDataH <- openFile rawdatafn ReadMode

       hSetBuffering stdin LineBuffering

       procData tmpdir encoder offsetH rawDataH
       hClose offsetH

procData :: FilePath -> String -> Handle -> Handle -> IO ()
procData tmpdir encoder offseth = 
    pdworker (tmpdir ++ "/sizefn") encoder offseth 0 (-1)

pdworker :: FilePath -> String -> Handle -> Int64 -> Integer -> Handle -> IO ()
pdworker sizefp encoder offseth bytesWritten lastBlock rawDataH =
    do eof <- hIsEOF stdin
       if eof
           then return ()
           else do l <- getLine
                   let thisInfo = parseMinusR . strip $ l
                   case thisSize of
                     (fp, Nothing) -> -- Last entry
                                 do newlen <- writeEncoded Nothing
                                    write newlen fp
                     (fp, Just sz) -> -- Regular entry
                             do let fullsize = sz * 512
                                newlen <- writeEncoded (Just (fromIntegral fullsize))
                                write newlen fp
                                pdworker sizefp encoder offseth (bytesWritten + newlen) rawDataH
    where write :: Int64 -> FilePath -> IO ()
          write l fp =
              hPutStrLn offseth $ show bytesWritten ++ "\t" ++ show l ++ "\t" ++
                        fp
          writeEncoded size =
              do newsz <- case size of
                            Nothing -> return Nothing
                            Just x -> do y <- evaluate x
                                         return (Just y)
                 runIO $ catBytesFrom 512 rawDataH newsz -|- encoder -|- countBytes sizefp
                 -- BSL.putStr x
                 countStr <- readFile sizefp
                 -- let countStr = "0"
                 -- hPutStrLn stderr $ "writeEncoded: got " ++ show countStr
                 return (read countStr)

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
parseMinusR = map parseMinusRLine . lines

parseMinusRLine :: String -> InputTarContent
parseMinusRLine l = 
    case parse entry ("Line: " ++ show l) l of
      Left x -> error (show x)
      Right y -> y
    where entry = do string "block "
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
