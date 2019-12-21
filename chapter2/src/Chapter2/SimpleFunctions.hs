module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if null lst1
                then lst2
                else (head lst1) : (tail lst1 +++ lst2)

reverse2 :: [a] -> [a]
reverse2 lst = if null lst
               then lst
               else reverse2 (tail lst) +++ [head lst]

maxmin :: Ord a => [a] -> (a, a)
maxmin list = let h = head list
              in if null (tail list)
                 then (h, h)
                 else (if h > t_max then h else t_max
                     , if h < t_min then h else t_min)
                     where t = maxmin(tail list)
                           t_max = fst t
                           t_min = snd t

data Person = Person String String deriving Show

data Client = GovOrg String
            | Company String Integer String String
            | Individual Person Bool
            deriving Show

clientName :: Client -> String
clientName client = case client of
                      GovOrg name -> name
                      Company name id person resp -> name
                      Individual person ads ->
                        case person of
                            Person fNm lNm -> fNm ++ " " ++ lNm

companyName :: Client -> Maybe String
companyName client = case client of
                        Company name _ _ _ -> Just name
                        _                  -> Nothing

