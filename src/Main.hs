{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Data.String.Conversions as C
import qualified Network.HTTP.Simple as H
import qualified Data.Text.Lazy.IO as TIOL
import qualified Data.Text.IO as TIO
import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML
import Text.XML.Cursor
import qualified Data.Default.Class as Def
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Control.Concurrent as C
import Data.Maybe
import Data.Bifunctor

data Example = Example {
  hangle :: T.Text,
  japanese :: T.Text
} deriving Show

(!?) :: [a] -> Int -> Maybe a
{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
    0 -> Just x
    _ -> r (k-1)) (const Nothing) xs n

br = "<br>" -- ankiで改行は<br>
bar = "" -- ankiで改行は<br>
spacing = br ++ bar ++ br

unlines' [] = []
unlines' (x:xs) = x ++ br ++ unlines' xs

-- タブ区切りでsplit, hかjの列に改行が含まれてたら死ぬが許容
splitByTab :: String -> [String]
splitByTab s = let (h, t) = split' s in h : (if length h == 0 then [] else splitByTab t)
  where
  split' :: String -> (String, String)
  split' t = (takeWhile (/= '\t') t, drop 1 $ dropWhile (/= '\t') t) 

-- 用例が載っているtableの要素を取得
parse :: TL.Text -> [Example]
parse t = let
  root = fromDocument $ HTML.parseLT t
  tables = root $// element "table" 
  example_table = tables !! 7
  examples = example_table $// element "td" 
  in pairUp . map extractText $ map node examples
  where
  pairUp (_:h:j:t) = Example h j : pairUp t
  pairUp _ = []

  extractText :: XML.Node -> T.Text
  extractText (XML.NodeElement (XML.Element _ _ nodes)) = foldl (\t n -> T.append t $ extractText n) "" nodes 
  extractText (XML.NodeContent t) = t
  extractText _ = ""

updateTexts :: (String -> String -> IO (String, String)) -> String -> IO String
updateTexts updater s = do
  let cols = splitByTab s
      h = cols !! 3 -- 韓国語
      j = cols !! 4 -- 日本語
      l = maybeToList $ cols !? 5 -- leechの場合のみこのカラムが存在する
  (nH, nJ) <- updater h j
  return . untabs $ take 3 cols ++ [nH, nJ] ++ l
  where
  untabs [] = []
  untabs (x:xs) = x ++ "\t" ++ untabs xs

-- 韓国語から用例を取得
getUseSamples :: String -> String -> IO (String, String)
getUseSamples h j = do
  url <- H.parseRequest $ "https://www.kpedia.jp/s/1/"  ++ h
  req <- H.httpBS url
  let body = C.convertString $ H.getResponseBody req
      examples = take example_limit $ parse body
      hExamples = unlines' $ map (C.convertString . hangle) examples
      jExamples = unlines' $ map (C.convertString . japanese) examples
  return (hExamples, jExamples)
  where
  example_limit = 5 -- exampleの数を制限

main :: IO ()
main = do
  f <- lines <$> (readFile =<< getLine)
  let header = take 6 f
      body = drop 6 f
  putStr $ unlines header
  mapM_ (\line -> do
    C.threadDelay (100 * 1000) -- 0.1秒sleep
    putStrLn =<< updateTexts (\h j -> bimap (\x -> h ++ spacing ++ x) (\x -> j ++ spacing ++ x) <$> getUseSamples h j) line) body
