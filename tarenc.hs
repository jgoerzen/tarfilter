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
import Data.IORef
import Control.Monad
import System.Posix.IO
import System.Posix.Process
import Control.Exception(evaluate)

type InputTarContent = (Int64, FilePath)
type InputTarSize = (FilePath, Maybe Int64)

main :: IO ()
main = 
    do --updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       (encoder, offsetfn) <- case argv of
                                [x, y] -> return (x, y)
                                _ -> usage
       
       offsetH <- openFile offsetfn WriteMode
       inpData <- BSL.getContents

       (inpcontent_str, scanr) <- scanInput inpData
       let sizes = convToSize . parseMinusR $ inpcontent_str

       procData encoder offsetH inpData sizes
       scanr >>= checkResults
       hClose offsetH

procData :: String -> Handle -> BSL.ByteString -> [InputTarSize] -> IO ()
procData encoder offseth = pdworker encoder offseth 0 

pdworker :: String -> Handle -> Int64 -> BSL.ByteString -> [InputTarSize] -> IO ()
pdworker _ _ _ _ [] = return ()
pdworker encoder offseth bytesWritten inp (thisSize:xs) =
    case thisSize of
      (fp, Nothing) -> -- Last entry
          do newlen <- writeEncoded inp
             write newlen fp
      (fp, Just sz) -> -- Regular entry
          do let fullsize = sz * 512
             let (thiswrite, remainder) = BSL.splitAt fullsize inp
             newlen <- writeEncoded thiswrite
             write newlen fp
             pdworker encoder offseth (bytesWritten + newlen) remainder xs
    where write :: Int64 -> FilePath -> IO ()
          write l fp =
              hPutStrLn offseth $ show bytesWritten ++ "\t" ++ show l ++ "\t" ++
                        fp
          writeEncoded x =
              do -- Ugly hack.  stdout fd 1 is messed with by the pipe.  We
                 -- dup it so we can write directly out AND return a byte
                 -- count.
                 aliasFd <- dup 1
                 aliasH <- fdToHandle aliasFd
                 count <- run $ echoBS x -|- encoder -|- countBytes aliasH
                 hClose aliasH
                 return (read count)

countBytes :: Handle -> BSL.ByteString -> IO BSL.ByteString
countBytes h inp = 
    do size <- foldM updSize 0 (BSL.toChunks inp)
       let retval = BSL.pack . map (fromIntegral . fromEnum) . show $ size
       evaluate (BSL.length retval)
       hClose h
       return retval
       
    where updSize accum c =
              do BS.hPutStr h c
                 return (accum + (fromIntegral . BS.length $ c))
           
usage =
    do putStrLn "Usage:\n"
       putStrLn "tarenc encoder outputoffsetfilename"
       putStrLn "input from stdin, output tar file is written to stdout"
       putStrLn "use /dev/null for outputoffsetfilename if you don't want offset info"
       fail "Usage error"
       
scanInput :: BSL.ByteString -> IO (String, IO (String, ProcessStatus))
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
