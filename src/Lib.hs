-- | test case generator for pretty combinators
-- https://github.com/haskell/pretty/issues/32#issuecomment-223213838

{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveGeneric #-}
{-# language ScopedTypeVariables #-}

module Lib where

import Text.PrettyPrint.HughesPJ
import Series
import GHC.Generics
import Data.Time.Clock
import Control.Monad ( forM, forM_, guard, replicateM )
import Data.List ( transpose, intersperse )
import Data.Char (toLower)


-- I'm pretty sure this could be generified, TH-ified etc.
-- (Excuse the longwinded code, I did not have time to write short one)

data Op = Hcat | Hsep | Vcat | Sep | Cat | Fsep | Fcat
  deriving (Eq, Ord, Show, Generic)

instance Pr Op where pr = text . map toLower . show

fun :: Op -> ([Doc] -> Doc)
fun op = case op of Hcat -> hcat ; Hsep -> hsep ; Vcat -> vcat ; Sep -> sep ; Cat -> cat ; Fsep -> fsep ; Fcat -> fcat

instance Serial Op

data Term = Leaf
       | Branch Op [Term]
  deriving (Eq, Ord, Generic)

eval :: Term -> Doc
eval t = case t of
  Leaf -> text "l"
  Branch op ts -> fun op $ map eval ts

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
check_context :: Int -> Context -> Term -> IO NominalDiffTime
check_context n ctx b = do
  let d = iterate (apply ctx) b !! n
  time_whnf ( length $ render $ eval d )

data Case = Case { c :: Context, b :: Term, n :: Int, t :: NominalDiffTime }

instance Pr Case where
  pr (Case c b n t) =
    text "length $ render $ iterate ( \\ hole -> " <+> pr c <+> text ")" <+> parens (pr b) <+> text "!!" <+> int n
    <+> text " -- " <+> text (show t)

instance Show Case where show = render . pr

-- | first argument: no. of contexts to generate,
-- second: max. iterations for each,
-- returns worst offender (with max total sum)
check_contexts :: Int -> Int -> IO Case
check_contexts gen it = do
  let go c [] = return c
      go c (ctx : ctxs) = do
        out <- check_context it ctx Leaf
        let c' = Case ctx Leaf it out
        if (t c' > t c) then do print c'; go c' ctxs else go c ctxs
  go (Case Hole Leaf 0 0) $ take gen $ contents series

