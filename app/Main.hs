{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent as C
import qualified Control.Lens as L
import qualified Network.Wreq as W
import qualified Text.Taggy.Lens as T
import qualified Data.Text as X
import qualified Data.Set as S
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Maybe (catMaybes)

url :: Int -> String
url id = "http://www.genealogy.ams.org/id.php?id=" ++ (show id)

vincent :: Int
vincent = 165242

gordon :: Int
gordon = 95717

vauquelin = 158571
bernoulli = 53410 -- 54440

-- Type for mathematicians
data M = M { mid :: Int
           , name :: X.Text
           , degrees :: [(X.Text, X.Text, X.Text)]
           , advisors :: [Int] }
  deriving Show

extract :: Int -> IO M
extract id = do
  response <- W.get (url id)
  let html = response L.^?! responseText . T.html
  return M { mid = id
           , name = extractName html
           , degrees = extractDegrees html
           , advisors = extractAdvisors html }

responseText = W.responseBody . L.to decodeUtf8

extractName :: T.Node -> X.Text
extractName html = X.unwords . X.words $ head name
  where name = html L.^.. T.allNamed (L.only "h2") . T.contents

extractDegrees :: T.Node -> [(X.Text, X.Text, X.Text)]
extractDegrees html = map extractDegree degspans
  where
    spans = html L.^.. T.allNamed (L.only "span")
    isDeg x = x L.^. T.children . L.to length == 3
    degspans = filter isDeg spans

extractDegree :: T.Element -> (X.Text, X.Text, X.Text)
extractDegree span = (X.strip deg, uni, X.strip year)
  where
    chlds = span L.^. T.children
    deg  = chlds !! 0 L.^. T.content
    uni  = chlds !! 1 L.^. T.element . T.contents
    year = chlds !! 2 L.^. T.content

extractAdvisors :: T.Node -> [Int]
extractAdvisors html = map (read . X.unpack) advIds
  where
    allPs = html L.^.. T.allNamed (L.only "p")
    advPs = filter isAdvisor allPs
    isAdvisor = X.isPrefixOf "Advisor" . L.view T.contents
    hrefs = advPs L.^.. traverse . T.allNamed (L.only "a") . T.attr "href"
    advIds = catMaybes $ map (>>= extractId) hrefs

extractId :: X.Text -> Maybe X.Text
extractId = X.stripPrefix "id.php?id="

recExtract :: S.Set Int -> [M] -> [Int] -> IO [M]
recExtract seen acc [] = return acc
recExtract seen acc (id:queue)
  | S.member id seen = recExtract seen acc queue
  | otherwise = do m <- extract id
                -- putStrLn $ show m
                   C.threadDelay 1000000
                   let seen' = S.insert id seen
                       queue' = queue ++ advisors m
                   recExtract seen' (m:acc) queue'

writeDot :: [M] -> IO ()
writeDot ms = do putStrLn "digraph {"
                 putStrLn "graph [bgcolor=grey16,pad=1]"
                 putStrLn $ "node [" ++ nodeAttrs ++ "]"
                 putStrLn "edge [color=brown2,penwidth=2]" -- gold
                 mapM_ printMthm ms
                 putStrLn "}"
  where
    nodeAttrs = "shape=box,style=\"rounded,filled\",color=white," ++
                "fontname=\"Helvetica\",fontsize=8"
    printMthm m = let id = mid m in
      do putStrLn $ (show id) ++ " [label=" ++ label m ++ "]"
         mapM_ printLink $ zip (advisors m) (repeat id)
 -- label m = "\"" ++ X.unpack (name m) ++ "\""
    label m = "<<table><tr><td colspan=\"3\">" ++
              "<font point-size=\"11\">" ++ fmtName (name m) ++
              "</font></td></tr>" ++ concatMap tr (degrees m) ++
              "</table>>"
    tr (deg, uni, year) =
      "<tr><td>" ++ fmtDeg deg  ++ "</td>" ++
          "<td>" ++ fmtUni uni  ++ "</td>" ++
          "<td>" ++ fmtYear year ++ "</td></tr>"
    printLink (m1, m2) = putStrLn $ show m1 ++ " -> " ++ show m2
    fmtName n = X.unpack . X.unwords $ f xs
      where xs = X.split (\c -> c == '(' || c == ')') n
            f [x1, x2, x3] = X.words x1 ++ X.words x3
            f [x1] = [x1]
            f _ = error "too many parenthesis"
    fmtDeg d = X.unpack $ X.replace ", " ",<br/>" d
    fmtUni u = X.unpack $ X.replace "and " "and<br/>" u
    fmtYear y = X.unpack $ if a == y then y else X.snoc a ' '
      where a = X.replace ", " ",<br/>" y

main :: IO ()
main = writeDot =<< recExtract (S.fromList [bernoulli, vauquelin]) [] [vincent]

