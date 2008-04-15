{- 
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
-}

module Scan(scan) where
import Types
import Control.Monad(liftM)
import System.Posix.Files
import System.Log.Logger
import Data.List

scan :: RunInfo -> [FilePath] -> IO [[Result]]
scan ri fplist =
    do sizes <- (liftM concat $ mapM getSize fplist)
       let func = if preserveOrder ri then binify_po else binify_opt
       case func bins sizes of
         Left x -> fail x
         Right x -> return x
    where getSize f = 
              do s <- getFileStatus f
                 if isRegularFile s
                    then return [(fromIntegral (fileSize s), f)]
                    else do warningM "scan" $ "Warning: file " ++ f ++ " is not a regular file; skipping"
                            return []
                    
          bins = firstBinSize ri : repeat (binSize ri)

binify_po :: (Num s, Ord s, Show o) => 
             [s]                -- ^ The sizes of bins.  List may be infinite.
          -> [(s, o)]           -- ^ (size, object) pairs
          -> Either String [[(s, o)]]         -- ^ Results or error
binify_po _ [] = Right []                     -- ^ Ran out of sizes
binify_po [] _ = Left "Ran out of bins"
binify_po (thisbinsize:otherbins) sizes =
    let fillBin _ [] = Right []
        fillBin accumsize ((s, o):xs) 
            | s > thisbinsize = Left $ "Size " ++ show s ++ " greater than bin size " ++ show thisbinsize ++ " at " ++ show o
            | s + accumsize > thisbinsize = Right []
            | otherwise = do next <- fillBin (accumsize + s) xs
                             return $ (s, o) : next
        in do thisset <- fillBin 0 sizes
              next <- binify_po otherbins (drop (length thisset) sizes)
              return (thisset : next)

binify_opt :: (Num s, Ord s, Show o, Ord o) => 
              [s]                -- ^ The sizes of bins.  List may be infinite.
           -> [(s, o)]           -- ^ (size, object) pairs
           -> Either String [[(s, o)]]         -- ^ Results or error
binify_opt bins sizes = binify_opt' bins (sort sizes)

binify_opt' :: (Num s, Ord s, Show o) => 
              [s]                -- ^ The sizes of bins.  List may be infinite.
           -> [(s, o)]           -- ^ (size, object) pairs
           -> Either String [[(s, o)]]         -- ^ Results or error
binify_opt' _ [] = Right []                     -- ^ Ran out of sizes
binify_opt' [] _ = Left "Ran out of bins"
binify_opt' (thisbinsize:otherbins) sizes =
    let fillBin _ [] = Right []
        fillBin accumsize sizelist =
            case break (\x -> (fst x) + accumsize < thisbinsize) sizelist of
              (_, []) ->
                  if accumsize == 0
                     then Left $ "No items small enough to fit in bin " ++ show thisbinsize ++ "; remainder is " ++ show sizelist
                     else Right []
              (nonmatches, ((s, o):matchxs)) ->
                  do next <- fillBin (accumsize + s) (nonmatches ++ matchxs)
                     return $ (s, o) : next
        in do thisset <- fillBin 0 sizes
              next <- binify_opt' otherbins (drop (length thisset) sizes)
              return (thisset : next)
