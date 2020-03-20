{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#include "version-compatibility-macros.h"

module Main (main) where



import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable
import           Data.Text           (Text)
import qualified Data.Text           as T
import           System.Random
import           Weigh

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import qualified Text.PrettyPrint.ANSI.Leijen          as WL

#if !(APPLICATIVE_MONAD)
import Control.Applicative
#endif



main :: IO ()
main = (mainWith . sequenceA_)
    [ benchOptimize
    , benchWLComparison
    ]

benchOptimize :: Weigh ()
benchOptimize = benchmark_ $!! randomShortWords
  where
    benchmark_ = \shortWords ->
        let doc = hsep (map pretty shortWords)
        in wgroup' "Many small words"
            [ func "Unoptimized"     renderLazy (layoutPretty defaultLayoutOptions               doc)
            , func "Shallowly fused" renderLazy (layoutPretty defaultLayoutOptions (fuse Shallow doc))
            , func "Deeply fused"    renderLazy (layoutPretty defaultLayoutOptions (fuse Deep    doc))
            ]

    randomShortWords :: [Text]
    randomShortWords = evalState (randomShortWords' 100) (mkStdGen 0)

    randomShortWords' :: Int -> State StdGen [Text]
    randomShortWords' n = replicateM n randomShortWord

    randomShortWord :: State StdGen Text
    randomShortWord = do
        g <- get
        let (l, g') = randomR (0, 5) g
            (gNew, gFree) = split g'
            xs = take l (randoms gFree)
        put gNew
        pure (T.pack xs)

benchWLComparison :: Weigh ()
benchWLComparison = wgroup' "vs. other libs"
    [ wgroup' "renderPretty"
        [ func "this, unoptimized"     (renderLazy . layoutPretty defaultLayoutOptions)               doc
        , func "this, shallowly fused" (renderLazy . layoutPretty defaultLayoutOptions) (fuse Shallow doc)
        , func "this, deeply fused"    (renderLazy . layoutPretty defaultLayoutOptions) (fuse Deep    doc)
        , func "ansi-wl-pprint"        (\d -> WL.displayS (WL.renderPretty 0.4 80 d) "") wlDoc
        ]
    , wgroup' "renderSmart"
        [ func "this, unoptimized"     (renderLazy . layoutSmart defaultLayoutOptions)               doc
        , func "this, shallowly fused" (renderLazy . layoutSmart defaultLayoutOptions) (fuse Shallow doc)
        , func "this, deeply fused"    (renderLazy . layoutSmart defaultLayoutOptions) (fuse Deep    doc)
        , func "ansi-wl-pprint"        (\d -> WL.displayS (WL.renderSmart 0.4 80 d) "") wlDoc
        ]
    , wgroup' "renderCompact"
        [ func "this, unoptimized"     (renderLazy . layoutCompact)               doc
        , func "this, shallowly fused" (renderLazy . layoutCompact) (fuse Shallow doc)
        , func "this, deeply fused"    (renderLazy . layoutCompact) (fuse Deep    doc)
        , func "ansi-wl-pprint"        (\d -> WL.displayS (WL.renderCompact d) "") wlDoc
        ]
    ]
  where
    doc :: Doc ann
    doc = let fun x = "fun" <> parens (softline <> x)
              funnn = chain 10 fun
          in funnn (sep (take 48 (cycle ["hello", "world"])))

    wlDoc :: WL.Doc
    wlDoc = let fun x = "fun" WL.<> WL.parens (WL.softline WL.<> x)
                funnn = chain 10 fun
            in funnn (WL.sep (take 48 (cycle ["hello", "world"])))

    chain n f = foldr (.) id (replicate n f)

wgroup' :: String -> [Weigh ()] -> Weigh ()
wgroup' x = wgroup x . sequenceA_
