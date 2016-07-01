-- | test case generator for pretty combinators
-- https://github.com/haskell/pretty/issues/32#issuecomment-223213838

{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveGeneric #-}
{-# language ScopedTypeVariables #-}

module Lib where

-- for outputs of our program:
import Text.PrettyPrint.HughesPJ 

-- for measuring their performance:
import qualified Text.PrettyPrint.HughesPJ as TPH 
import qualified Text.PrettyPrint.Leijen as TPL
import qualified Text.PrettyPrint.Free as TPF

import Series
import GHC.Generics
import Data.Time.Clock
import Control.Monad ( forM, forM_, guard, replicateM )
import Data.List ( transpose, intersperse )
import Data.Char (toLower)

import Data.Proxy

-- I'm pretty sure this could be generified, TH-ified etc.
-- (Excuse the longwinded code, I did not have time to write short one)

data Op = Hcat | Hsep | Vcat | Sep | Cat | Fsep | Fcat
  deriving (Eq, Ord, Show, Generic)

instance Pr Op where pr = text . map toLower . show

instance Serial Op

data Term = Leaf
       | Branch Op [Term]
  deriving (Eq, Ord, Generic)

class Eval d where
  eval :: Term -> d
  size :: d -> Int

instance Eval TPH.Doc where
  size = length . render
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

instance Eval (TPF.Doc ()) where
  size d = length $ TPF.displayS ( TPF.renderPretty 0.4 80 d ) ""
  eval t = case t of
    Leaf -> TPF.text "l"
    Branch op ts ->
      let fun = case op of Hcat -> TPF.hcat ; Hsep -> TPF.hsep ; Vcat -> TPF.vcat ; Sep -> TPF.sep ; Cat -> TPF.cat ; Fsep -> TPF.fillSep ; Fcat -> TPF.fillCat
      in  TPF.align $ fun $ map eval ts

class Pr a where pr :: a -> Doc

instance Pr Term where
  pr t = case t of
    Leaf -> text "text " <+> doubleQuotes (text "l")
    Branch op ts -> pr op <+> brackets (hsep $ punctuate comma $ map pr ts)

instance Show Term where show = render . pr

instance Serial Term

data Context = Hole
       | CBranch Op [Term] Context [Term]
  deriving (Eq, Ord, Generic)

instance Serial Context

instance Pr Context where
  pr c = case c of
    Hole -> text "hole"
    CBranch op pre c post ->
      pr op <+> brackets (hsep $ punctuate comma $ map pr pre ++ [pr c] ++ map pr post)

instance Show Context where show = render . pr

apply :: Context -> Term -> Term
apply c t = case c of
  Hole -> t
  CBranch op xs c' ys -> Branch op $ xs ++ [apply c' t] ++ ys


-- | will force argument to whnf and measure the time to do this 
time_whnf :: a -> IO NominalDiffTime
time_whnf x = do
  start <- getCurrentTime
  end <- x `seq` getCurrentTime
  return $ diffUTCTime end start

-- | first argument: number of iterations of this context
check_context :: Eval d => Proxy d -> Int -> Context -> Term -> IO NominalDiffTime
check_context p n ctx b = do
  let d = iterate (apply ctx) b !! n
  time_whnf ( size ( asProxyTypeOf (eval d) p ) )

-- | just count how many textdetails were emitted
norender :: Doc -> Int
norender = fullRender PageMode 100 1.5 ( \ _ c -> succ c ) 0 


data Case = Case { c :: Context, b :: Term, n :: Int, t :: NominalDiffTime }

instance Pr Case where
  pr (Case c b n t) =
    text "length $ render $ iterate ( \\ hole -> " <+> pr c <+> text ")" <+> parens (pr b) <+> text "!!" <+> int n
    <+> text " -- " <+> text (show t)

instance Show Case where show = render . pr

-- | first argument: no. of contexts to generate,
-- second: max. iterations for each,
-- returns worst offender (with max total sum)
check_contexts :: Eval d => Proxy d -> Int -> IO Case
check_contexts p it = do
  let go c [] = return c
      go c (ctx : ctxs) = do
        out <- check_context p it ctx Leaf
        let c' = Case ctx Leaf it out
        if (t c' > t c) then do print c'; go c' ctxs else go c ctxs
  go (Case Hole Leaf 0 0) $ contents series

