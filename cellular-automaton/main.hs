import           Binary

type Model = [Bin]
type Queue = [Bin]
type Rule = (Int -> Bin)

-- サンプル初期モデル
initModel :: Model
initModel = genBins [0,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,1,0]

-- 0〜255の値 -> 8bitの値を次の時刻のバイナリ値に変換するRule
rule :: Int -> Rule
rule i = (get8bitData i !!)

-- Ruleに基づいてModelを次の時刻のModelに更新する
update :: Rule -> Model -> Model
update r m = snd (foldl f ([],[]) (last m : m ++ [head m]))
  where
    -- 長さが3であるか
    len3 q = length q == 3
    -- 要素上限3のキューにデータをセットする。
    setV v q = if len3 q then tail q ++ [v] else q ++ [v]
    -- キューが3個溜まっていたら、ruleに基づいた新しい値をm'にセットする。
    f :: (Queue, Model) -> Bin -> (Queue, Model)
    f (q,m') b = let q' = setV b q in if len3 q' then (q', m' ++ [r $ toInt $ reverse q']) else (q', m')

-- updateを任意回数実行
updates :: Rule -> Int -> Model -> [Model]
updates r step iniModel = take step $ iterate (update r) iniModel

-- Modelを文字列にする
toStr :: Model -> String
toStr = map (\b -> if getV b == 1 then 'V' else '_')

-- Modelを画面に表示する
view :: Model -> IO ()
view = putStrLn . toStr

-- Modelの配列を画面に表示する
views :: [Model] -> IO ()
views = mapM_ view

