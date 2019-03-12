import Data.Char
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    polymerText <- fmap Text.lines (Text.readFile "5.txt")
    let polymerString =  head $ map Text.unpack polymerText
        allReactions = iterate reactPolymer polymerString
    putStrLn $ show $ length $ completelyReact polymerString 

testPolymer = "dabAcCaCBAcCcaDA"

-- Test if two elements are reactable, by simple check of case

isReactable :: Char -> Char -> Bool
isReactable a b 
    | a /= b && (toLower a) == b = True
    | a /= b && a == (toLower b) = True
    | otherwise                  = False

-- Function to do one time pass of polymer and react adjacent elements
reactPolymer :: [Char] -> [Char]
reactPolymer []       = []                          -- The empty string has no reactions
reactPolymer [a]      = [a]                         -- A singleton list also has no reactions 
reactPolymer (a:b:cs)                               -- Take first two chemicals off the list
    | isReactable a b = reactPolymer cs             -- If they react(cancel out) then thrown them away and carry on with the rest of polymer
    | otherwise       = (a:reactPolymer (b:cs))     -- Otherwise put the first char back on the front, and start from the next position

-- Use `until` to repeatedly apply reactPolymer until no more changes
completelyReact :: [Char] -> [Char]
completelyReact polymer =  until (\x -> reactPolymer x == x) reactPolymer polymer
