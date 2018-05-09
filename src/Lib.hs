module Lib where
  
import Control.Monad
import Data.Char
import Text.Read
import Data.Maybe
import Data.List
import Data.Foldable

digits = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]
teens = ["ten", "eleven", "tweleve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
thousands = ["", "thousand", "million", "billion"]

questions = ["0","1","0.1","1.","0.",".34","0.3456789","1.0","1.01","1000456.13"]
expected =     
  [ "zero dollars and zero cents"
  , "one dollar and zero cents"
  , "zero dollars and ten cents"
  , "one dollar and zero cents"
  , "zero dollars and zero cents"
  , "zero dollars and thirty-four cents"
  , "zero dollars and thirty-four cents"
  , "one dollar and zero cents"
  , "one dollar and one cent"
  , "one million four hundred and fifty-six dollars and thirteen cents" ]

type Digit = Int
data Amount = Amount Int Int

instance Show Amount where
  show (Amount d c) = intercalate " " [with_zero dollars, ds, "and", with_zero cents, cs]
    where
      ds = if d == 1 then "dollar" else "dollars"
      cs = if c == 1 then "cent"   else "cents"
      thousand_blocks = zipWith show000 (take3s $ digits d) [n,(n-1)..0]
      n = div (length (digits d) - 1) 3
      dollars = intercalate " " . filter (not.null) $ thousand_blocks
      cents = show00 $ digits c

      digits = map digitToInt . show
      with_zero x = if (null x) then "zero" else x

main :: IO ()
main = print . and $ zipWith (==) (map dollars questions) expected

dollars :: String -> String
dollars = show . parse_amount

take3s :: [a] -> [[a]]
take3s nums = if null bs then [as] else as : take3s bs
  where
    (as, bs) = splitAt m nums
    m = case mod (length nums) 3 of
      0 -> 3
      x -> x

show000 :: [Digit] -> Int -> String
show000 num k = case num of
  []        -> ""
  [z]       -> append_thousand $ digits !! (mod z 10)
  [y, z]    -> append_thousand $ show00 [y, z]
  [x, y, z] -> append_thousand $ append_hundred (digits !! x) ++ show00 [y, z]
  where
    append_nonnull :: String -> String -> String
    append_nonnull s x = if null x then "" else x++s
    append_thousand = append_nonnull $ if k == 0 then "" else " " ++ (thousands !! k)
    append_hundred = append_nonnull " hundred and "

show00 :: [Digit] -> String
show00 num = case num of
  [y]   -> digits !! y
  [0,y] -> digits !! y
  [1,y] -> teens  !! y
  [x,0] -> tens   !! x
  [x,y] -> (tens !! x) ++"-"++ (digits !! y)

parse_amount :: String -> Amount
parse_amount str =  Amount (parse_dollar dstr) (parse_cents cstr)
  where
    parse_dollar s = parse_int $ filter isDigit s
    parse_cents  s = parse_int . take 2 $ (filter isDigit s) ++ repeat '0'
    (dstr, cstr) = span (/='.') str
    parse_int = fromMaybe 0 . readMaybe
    