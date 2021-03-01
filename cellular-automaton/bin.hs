module Binary ( Bin
              , genBin
              , genBins
              , getV
              , get8bitData
              , sumBins
              , toInt) where

-- バイナリ値型
newtype Bin = B Int deriving (Show,Eq)

-- バイナリ値型を生成する。
genBin :: Int -> Bin
genBin i
  | i == 0    = B 0
  | i == 1    = B 1
  | otherwise = error "Arugment error.0 or 1"

genBins :: [Int] -> [Bin]
genBins = map genBin

-- バイナリ値型からInt値を取り出す。
getV :: Bin -> Int
getV (B x) = x

-- 0〜255の数値を8bit表現のバイナリ値型配列に変換する。
get8bitData :: Int -> [Bin]
get8bitData v
  | v < 0 || 255 < v = error "Arugment error: Range out."
  | otherwise        = let (_,b) = binary (v,[]) in (reverse .map genBin) $ replicate (8 - length b) 0 ++ b
  where
    binary :: (Int,[Int]) -> (Int, [Int])
    binary (n,xs)
      | n == 1    = (1, 1:xs)
      | n == 0    = (0, 0:xs)
      | otherwise = binary (n `div` 2, (n `mod` 2) : xs)

-- バイナリ値型の配列の合計値
sumBins :: [Bin] -> Int
sumBins = sum . map getV

-- バイナリ値をInt型に変換する。
toInt :: [Bin] -> Int
toInt bs = sum $ zipWith (curry  (\v -> 2 ^ fst v * (getV .snd) v)) [0..] bs
