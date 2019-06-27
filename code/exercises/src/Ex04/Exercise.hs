{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ex04.Exercise where

import qualified Data.Map as Map
import Data.Text (Text)

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex04.Common
import Ex04.Run

ex04 ::
  forall t.
  Reflex t =>
  Inputs t ->
  Outputs t
ex04 (Inputs bMoney bCarrot bCelery bCucumber bSelected eBuy eRefund) =
  let
    products = [carrot, celery, cucumber]

    stockSingleton :: Stock -> Map.Map Text Stock
    stockSingleton stock@(Stock product _) = Map.singleton (pName product) stock

    bProductStock :: Behavior t (Map.Map Text Stock)
    bProductStock = mconcat [ stockSingleton <$> bCarrot
                            , stockSingleton <$> bCelery
                            , stockSingleton <$> bCucumber
                            ]

    eStock :: Event t Stock
    eStock = fmapMaybe id $ Map.lookup <$> bSelected <*> bProductStock <@ eBuy

    checkNotEnoughMoney money (Stock (Product _ cost) _) = money < cost
    checkNotEnoughStock (Stock _ 0) = True
    checkNotEnoughStock _ = False

    eError =
      leftmost [ ItemOutOfStock <$ ffilter checkNotEnoughStock eStock
               , NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> bMoney <@> eStock)
               ]

    eVend =
      pName . sProduct <$> difference eStock eError

    eSpend =
      pCost . sProduct <$> difference eStock eError

    eChange =
      bMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex04
#endif

