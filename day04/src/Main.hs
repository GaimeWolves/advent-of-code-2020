module Main where

import System.Environment ( getArgs )
import Data.List.Split ( splitOn, splitOneOf )
import Data.Maybe ( fromMaybe, isJust )
import Text.Regex.TDFA ( (=~) )

data Passport = Passport
  { byr :: Maybe String -- Birth year
  , iyr :: Maybe String -- Issue year
  , eyr :: Maybe String -- Expiration year
  , hgt :: Maybe String -- Height
  , hcl :: Maybe String -- Hair color
  , ecl :: Maybe String -- Eye color
  , pid :: Maybe String -- Passport ID
  , cid :: Maybe String -- Country ID
  } deriving (Show)

initPassport :: Passport
initPassport = Passport
  { byr = Nothing
  , iyr = Nothing
  , eyr = Nothing
  , hgt = Nothing
  , hcl = Nothing
  , ecl = Nothing
  , pid = Nothing
  , cid = Nothing
  }

readAttribute :: String -> Passport -> Passport
readAttribute a p
  | attr == "byr" = p { byr = Just val }
  | attr == "iyr" = p { iyr = Just val }
  | attr == "eyr" = p { eyr = Just val }
  | attr == "hgt" = p { hgt = Just val }
  | attr == "hcl" = p { hcl = Just val }
  | attr == "ecl" = p { ecl = Just val }
  | attr == "pid" = p { pid = Just val }
  | attr == "cid" = p { cid = Just val }
  | otherwise = error "Invalid passport attribute"
  where
    (attr:val:_) = splitOn ":" a

readPassport :: [String] -> Passport
readPassport = foldr readAttribute p
  where p = initPassport

readFile' :: String -> IO [Passport]
readFile' f = do
  contents <- readFile f
  return (map (readPassport . splitOneOf " \n") (splitOn "\n\n" contents))

isValid :: Passport -> Bool
isValid p = isByr && isIyr && isEyr && isHgt && isHcl && isEcl && isPid
  where
    isByr = isJust (fromMaybe Nothing (Just (byr p)))
    isIyr = isJust (fromMaybe Nothing (Just (iyr p)))
    isEyr = isJust (fromMaybe Nothing (Just (eyr p)))
    isHgt = isJust (fromMaybe Nothing (Just (hgt p)))
    isHcl = isJust (fromMaybe Nothing (Just (hcl p)))
    isEcl = isJust (fromMaybe Nothing (Just (ecl p)))
    isPid = isJust (fromMaybe Nothing (Just (pid p)))

isByrValid :: String -> Bool
isByrValid v = n >= 1920 && n <= 2002
  where n = read v

isIyrValid :: String -> Bool
isIyrValid v = n >= 2010 && n <= 2020
  where n = read v

isEyrValid :: String -> Bool
isEyrValid v = n >= 2020 && n <= 2030
  where n = read v

isHgtValid :: String -> Bool
isHgtValid v
  | u == "in" = n >= 59 && n <= 76
  | u == "cm" = n >= 150 && n <= 193
  | otherwise = False
  where (n, u) = head (reads v :: [(Int, String)])

isHclValid :: String -> Bool
isHclValid v = v =~ "^#[0-9a-f]{6}$"

isEclValid :: String -> Bool
isEclValid v = v =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"

isPidValid :: String -> Bool
isPidValid v = v =~ "^[0-9]{9}$"

isValid2 :: Passport -> Bool
isValid2 p = isByr && isIyr && isEyr && isHgt && isHcl && isEcl && isPid
  where
    isByr = isByrValid (fromMaybe "" (byr p))
    isIyr = isIyrValid (fromMaybe "" (iyr p))
    isEyr = isEyrValid (fromMaybe "" (eyr p))
    isHgt = isHgtValid (fromMaybe "" (hgt p))
    isHcl = isHclValid (fromMaybe "" (hcl p))
    isEcl = isEclValid (fromMaybe "" (ecl p))
    isPid = isPidValid (fromMaybe "" (pid p))

main :: IO ()
main = do
  [file] <- getArgs
  p <- readFile' file

  print (length (filter isValid p))
  print (length (filter isValid2 (filter isValid p)))