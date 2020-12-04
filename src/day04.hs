import Data.Char
import Data.List
import Data.List.Split

toKeyValuePair :: String -> (String, String)
toKeyValuePair xs = (a, b)
  where
    [a, b] = splitOn ":" xs

toKeyValuePairs :: String -> [(String, String)]
toKeyValuePairs = map toKeyValuePair . (=<<) words . lines

requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validBirthYear :: String -> Bool
validBirthYear s = null xs && x >= 1920 && x <= 2002
  where
    [(x, xs)] = reads s

validIssuerYear :: String -> Bool
validIssuerYear s = null xs && x >= 2010 && x <= 2020
  where
    [(x, xs)] = reads s

validExpirationYear :: String -> Bool
validExpirationYear s = null xs && x >= 2020 && x <= 2030
  where
    [(x, xs)] = reads s

validHeight :: String -> Bool
validHeight s = (xs == "cm" && x >= 150 && x <= 193) || (xs == "in" && x >= 59 && x <= 76)
  where
    [(x, xs)] = reads s

isHexValue :: Char -> Bool
isHexValue x = isDigit x || x >= 'a' && x <= 'f'

validHairColour :: String -> Bool
validHairColour s = length s == 7 && x == '#' && all isHexValue xs
  where
    (x : xs) = s

validEyeColour :: String -> Bool
validEyeColour s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPassportId :: String -> Bool
validPassportId s = length s == 9 && all isDigit s

hasRequiredFields :: [(String, String)] -> Bool
hasRequiredFields = (==) [] . (\\) requiredKeys . map fst

isValidField :: (String, String) -> Bool
isValidField ("byr", xs) = validBirthYear xs
isValidField ("iyr", xs) = validIssuerYear xs
isValidField ("eyr", xs) = validExpirationYear xs
isValidField ("hgt", xs) = validHeight xs
isValidField ("hcl", xs) = validHairColour xs
isValidField ("ecl", xs) = validEyeColour xs
isValidField ("pid", xs) = validPassportId xs
isValidField _ = True

isValid :: [(String, String)] -> Bool
isValid xs = hasRequiredFields xs && all isValidField xs

parse :: String -> [[(String, String)]]
parse = map toKeyValuePairs . splitOn "\n\n"

main = interact $ show . length . filter isValid . parse