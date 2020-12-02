data PasswordPolicy = Policy
  { p1 :: Int,
    p2 :: Int,
    character :: Char,
    password :: String
  }
  deriving (Show)

readsInt :: ReadS Int
readsInt = reads

instance Read PasswordPolicy where
  readsPrec _ xs =
    [ ( Policy
          { p1 = m1,
            p2 = m2,
            character = c,
            password = zzs
          },
        ""
      )
    ]
    where
      [(m1, '-' : ys)] = readsInt xs
      [(m2, ' ' : zs)] = readsInt ys
      (c : ':' : ' ' : zzs) = zs

isValid :: PasswordPolicy -> Bool
isValid p = (c == c1) /= (c == c2)
  where
    m1 = p1 p
    m2 = p2 p
    c = character p
    ps = password p
    c1 = ps !! (m1 - 1)
    c2 = ps !! (m2 - 1)

main = interact $ show . length . filter (== True) . map (isValid . read) . lines