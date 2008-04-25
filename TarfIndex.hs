{- 
Copyright (C) 2008 John Goerzen <jgoerzen@complete.org>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module TarfIndex where
import Text.ParserCombinators.Parsec

data TarEntry = 
   TarEntry {uncOff :: Integer,
            cmpOff :: Integer,
            uncSize :: Integer,
            cmpSize :: Integer,
            tarFileName :: String}
   deriving (Eq, Read, Show)

instance Ord TarEntry where
    compare a b = compare (cmpOff a) (cmpOff b)

type TarFile = [TarEntry]

magicHeader :: String
magicHeader = "uncoff\tcmpoff\tuncsize\tcmpsize\tfilename"

formatIndex :: TarFile -> String
formatIndex tf = unlines (magicHeader : map formatEntry tf)

formatEntry :: TarEntry -> String
formatEntry te =
    printf "%d\t%d\t%d\t%d\t%s" (uncOff te) (cmpOff te) (uncSize te)
           (cmpSize te) (tarFileName te)

parseIndex :: String -> TarFile
parseIndex str =
    let tarfile = do header
                     many entry
                     eof
        header = do string magicHeader
                    eol
        eol = char '\n'
        entry = do uncoffs <- numericField
                   cmpoffs <- numericField
                   uncsizes <- numericField
                   cmpsizes <- numericField
                   filename <- many1 (noneOf "\n")
                   eol
                   return $ TarEntry {unCoff = read uncoffs,
                                      cmpOff = read cmpoffs,
                                      uncSize = read uncsizes,
                                      cmpSize = read cmpsizes,
                                      tarFileName = filename}
        numericField = do r <- many1 digit
                          char '\t'
                          return r
    in
    case parse tarfile "(index)" str of
      Left x -> error (show x)
      Right y -> y
