{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import           Weigh
import           Control.DeepSeq
import           Data.Char
import           Data.Coerce
import           Data.Foldable
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Prettyprint.Doc.Internal



-- The old implementation. Performance isn’t much worse to be honest, mostly
-- well within a σ.
alternative :: Text -> Doc ann
alternative t = case T.length t of
    0 -> Empty
    1 -> Char (T.head t)
    n -> Text n t

current :: Text -> Doc ann
current = unsafeTextWithoutNewlines

main :: IO ()
main = (mainWith . sequenceA_) [ weighText (letters n) | n <- [0,1,2,3,5,10,50,100] ]

letters :: Int -> Text
letters n = T.pack (take n (filter isAlpha [minBound ..]))

weighText :: Text -> Weigh ()
weighText input = wgroup' (show (pretty (T.length input) <+> plural "letter" "letters" (T.length input)))
    [ wfunc "alternative" alternative input
    , wfunc "current" current input ]

wgroup' :: String -> [Weigh ()] -> Weigh ()
wgroup' x = wgroup x . sequenceA_

newtype Whnf a = Whnf { unWhnf :: a }

instance NFData (Whnf a) where rnf = flip seq ()

wfunc :: ∀ a b . String -> (b -> a) -> b -> Weigh ()
wfunc s f = func s (coerce f :: Whnf b -> Whnf a) . Whnf
