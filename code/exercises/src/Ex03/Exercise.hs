{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Ex03.Exercise where

import qualified Data.Map as Map
import Data.Text(Text)

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex03.Common
import Ex03.Run

ex03 ::
  forall t.
  Reflex t =>
  Inputs t ->
  Outputs t
ex03 (Inputs bMoney bSelected eBuy eRefund) =
  let
    products = [carrot, celery, cucumber]

    -- productSingleton p = Map.singleton (pName p) (pure p) -- Map Text (Behavior t Product)
    -- productsMap = sequence $ foldMap productSingleton products

    productsMap :: Reflex t => Behavior t (Map.Map Text Product)
    productsMap = pure $ Map.fromList $ fmap (\p -> (pName p, p)) products

    eMaybeProduct = Map.lookup <$> bSelected <*> productsMap <@ eBuy
    eProduct = fmapMaybe id eMaybeProduct

    -- nameToProduct "Carrot" = Just carrot
    -- nameToProduct "Celery" = Just celery
    -- nameToProduct "Cucumber" = Just cucumber

    -- eProduct = attachWithMaybe (\n _ -> nameToProduct n) bSelected eBuy
    checkNotEnoughMoney money p = money < pCost p
    eNotEnoughMoney = checkNotEnoughMoney <$> bMoney <@> eProduct
    eError = NotEnoughMoney <$ ffilter id eNotEnoughMoney

    eSale = difference eProduct eNotEnoughMoney

    eVend =
      leftmost ["Insufficient funds" <$ eNotEnoughMoney, pName <$> eSale]
    eSpend =
      pCost <$> eSale
    eChange =
      bMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex03
#endif
