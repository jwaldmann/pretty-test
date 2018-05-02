-- | test case generator for pretty combinators
-- https://github.com/haskell/pretty/issues/32#issuecomment-223213838

{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveGeneric #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language AllowAmbiguousTypes #-}

module Lib where

-- for outputs of our program:
import qualified Text.PrettyPrint.HughesPJ as Doc

-- for measuring their performance:
import qualified Text.PrettyPrint.HughesPJ as TPH 
import qualified Text.PrettyPrint.Leijen as TPL
import qualified Text.PrettyPrint.Leijen.Text as TPLT
import qualified Text.PrettyPrint.Free as TPF
import qualified Text.PrettyPrint.ANSI.Leijen as TPAL
import qualified Text.PrettyPrint.Mainland as TPM
import qualified Data.Text.Prettyprint.Doc as DTP
import qualified Data.Text.Prettyprint.Doc.Render.Text as DTP

import Series
import GHC.Generics
import Data.Time.Clock
import System.Timeout
import Control.Monad ( forM, forM_, guard, replicateM )
import Data.List ( transpose, intersperse )
import Data.Char (toLower)
import qualified Data.Text.Lazy as DTL

data Proxy (t :: Tag) = Proxy

data Tag = TPH | TPL | TPLT | TPF | TPAL | TPM | DTP
  deriving (Eq, Ord, Show)

class Printer ( p :: Tag ) where type Doc p

instance Printer TPH where type Doc TPH = TPH.Doc
instance Printer TPL where type Doc TPL = TPL.Doc
instance Printer TPLT where type Doc TPLT = TPLT.Doc
instance Printer TPF where type Doc TPF = TPF.Doc ()
instance Printer TPAL where type Doc TPAL = TPAL.Doc
instance Printer TPM where type Doc TPM = TPM.Doc
instance Printer DTP where type Doc DTP = DTP.Doc ()

-- I'm pretty sure this could be generified, TH-ified etc.

data Op = Hcat | Hsep | Vcat | Sep | Cat | Fsep | Fcat
  deriving (Eq, Ord, Show, Generic)

instance Pr Op where pr = Doc.text . map toLower . show

instance Serial Op
 where series = Series $ [Hsep, Sep] : repeat []

data Term = Leaf
       | Branch Op [Term]
  deriving (Eq, Ord, Generic)

class Eval d where
  eval :: Term -> d
  size :: d -> Int

instance Eval TPH.Doc where
  size = length . TPH.render
  eval t = case t of
    Leaf -> TPH.text "l"
    Branch op ts ->
      let fun = case op of Hcat -> TPH.hcat ; Hsep -> TPH.hsep ; Vcat -> TPH.vcat ; Sep -> TPH.sep ; Cat -> TPH.cat ; Fsep -> TPH.fsep ; Fcat -> TPH.fcat
      in  fun $ map eval ts

instance Eval TPL.Doc where
  size d = length $ TPL.displayS ( TPL.renderPretty 0.4 80 d ) ""
  eval t = case t of
    Leaf -> TPL.text "l"
    Branch op ts ->
      let fun = case op of Hcat -> TPL.hcat ; Hsep -> TPL.hsep ; Vcat -> TPL.vcat ; Sep -> TPL.sep ; Cat -> TPL.cat ; Fsep -> TPL.fillSep ; Fcat -> TPL.fillCat
      in  TPL.align $ fun $ map eval ts

instance Eval TPLT.Doc where
  size d = fromIntegral $ DTL.length $ TPLT.displayT ( TPLT.renderPretty 0.4 80 d )
  eval t = case t of
    Leaf -> TPLT.text $ DTL.pack "l"
    Branch op ts ->
      let fun = case op of Hcat -> TPLT.hcat ; Hsep -> TPLT.hsep ; Vcat -> TPLT.vcat ; Sep -> TPLT.sep ; Cat -> TPLT.cat ; Fsep -> TPLT.fillSep ; Fcat -> TPLT.fillCat
      in  TPLT.align $ fun $ map eval ts

instance Eval (TPF.Doc ()) where
  size d = length $ TPF.displayS ( TPF.renderPretty 0.4 80 d ) ""
  eval t = case t of
    Leaf -> TPF.text "l"
    Branch op ts ->
      let fun = case op of Hcat -> TPF.hcat ; Hsep -> TPF.hsep ; Vcat -> TPF.vcat ; Sep -> TPF.sep ; Cat -> TPF.cat ; Fsep -> TPF.fillSep ; Fcat -> TPF.fillCat
      in  TPF.align $ fun $ map eval ts

instance Eval TPAL.Doc where
  size d = length $ TPAL.displayS ( TPAL.renderPretty 0.4 80 d ) ""
  eval t = case t of
    Leaf -> TPAL.text "l"
    Branch op ts ->
      let fun = case op of Hcat -> TPAL.hcat ; Hsep -> TPAL.hsep ; Vcat -> TPAL.vcat ; Sep -> TPAL.sep ; Cat -> TPAL.cat ; Fsep -> TPAL.fillSep ; Fcat -> TPAL.fillCat
      in  TPAL.align $ fun $ map eval ts

instance Eval TPM.Doc where
  size d = length $ TPM.displayS ( TPM.render 80 d ) ""
  eval t = case t of
    Leaf -> TPM.text "l"
    Branch op ts ->
      let fun = case op of Hcat -> TPM.stack ; Hsep -> TPM.spread ; Vcat -> TPM.stack ; Sep -> TPM.sep ; Cat -> TPM.cat ; Fsep -> TPM.spread ; Fcat -> TPM.stack
      in  TPM.align $ fun $ map eval ts

instance Eval (DTP.Doc ()) where
  size d = fromIntegral $ DTL.length $ DTP.renderLazy $ DTP.layoutPretty DTP.defaultLayoutOptions d
  eval t = case t of
    Leaf -> DTP.pretty "l"
    Branch op ts ->
      let fun = case op of Hcat -> DTP.hcat ; Hsep -> DTP.hsep ; Vcat -> DTP.vcat ; Sep -> DTP.sep ; Cat -> DTP.cat ; Fsep -> DTP.fillSep ; Fcat -> DTP.fillCat
      in  DTP.align $ fun $ map eval ts

class Pr a where pr :: a -> Doc.Doc

instance Pr Term where
  pr t = case t of
    Leaf -> Doc.text "text " Doc.<+> Doc.doubleQuotes (Doc.text "l")
    Branch op ts -> pr op Doc.<+> Doc.brackets (Doc.hsep $ Doc.punctuate Doc.comma $ map pr ts)

instance Show Term where show = Doc.render . pr

instance Serial Term

data Context = Hole
       | CBranch Op [Term] Context [Term]
  deriving (Eq, Ord, Generic)

instance Serial Context

instance Pr Context where
  pr c = case c of
    Hole -> Doc.text "hole"
    CBranch op pre c post ->
      pr op Doc.<+> Doc.brackets (Doc.hsep $ Doc.punctuate Doc.comma $ map pr pre ++ [pr c] ++ map pr post)

instance Show Context where show = Doc.render . pr

apply :: Context -> Term -> Term
apply c t = case c of
  Hole -> t
  CBranch op xs c' ys -> Branch op $ xs ++ [apply c' t] ++ ys

-- | will force argument to whnf and measure the time to do this 
time_whnf :: Int -> a -> IO Time
time_whnf to x = do
  start <- getCurrentTime
  mx <- timeout (to * 10^6) ( x `seq` return x )
  end <- getCurrentTime
  return $ case mx of
      Just x -> Exactly $ diffUTCTime end start
      Nothing -> Morethan $ diffUTCTime end start

-- | first argument: number of iterations of this context
check_context :: Eval (Doc d) => Proxy d -> Int -> Int -> Context -> Term -> IO Time
check_context (_ :: Proxy d) n to ctx b = do
  let d = iterate (apply ctx) b !! n
  time_whnf to ( size ( (eval d) :: Doc d  ) )

-- | just count how many textdetails were emitted
norender :: Doc.Doc -> Int
norender = Doc.fullRender Doc.PageMode 100 1.5 ( \ _ c -> succ c ) 0 


data Case = Case { c :: Context, b :: Term, n :: Int, t :: Time }

instance Pr Case where
  pr (Case c b n t) =
    Doc.text "length $ render $ iterate ( \\ hole -> "
    Doc.<+> pr c
    Doc.<+> Doc.text ")"
    Doc.<+> Doc.parens (pr b)
    Doc.<+> Doc.text "!!"
    Doc.<+> Doc.int n
    Doc.<+> Doc.text "-- "
    Doc.<+> Doc.text (show t)

instance Show Case where show = Doc.render . pr

data Time = Exactly NominalDiffTime | Morethan NominalDiffTime
  deriving (Eq, Ord, Show)

-- | first argument: no. of contexts to generate,
-- second: iterations for each,
-- third: timeout (in seconds)
-- returns worst offender (with max total sum)
check_contexts :: Eval (Doc d) => Proxy d -> Int -> Int -> IO Case
check_contexts (p :: Proxy d) it to = do
  let go c [] = return c
      go c (ctx : ctxs) = do
        out <- check_context p it to ctx Leaf
        let c' = Case ctx Leaf it out
        if (t c' > t c) then do print c'; go c' ctxs else go c ctxs
  go (Case Hole Leaf 0 (Exactly 0)) $ contents series


