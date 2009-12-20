module TestUtils(closeEnough, closeEnough2)
where

closeEnough x y e = x > y - e && x < y + e

closeEnough2 e x y = fst x > fst y - e && fst x < fst y + e &&
                     snd x > snd y - e && snd x < snd y + e

