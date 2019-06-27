{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ex05.Exercise where

import qualified Data.Map as Map
import Data.Text (Text)

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex05.Common
import Ex05.Run

ex05 ::
  forall t.
  Reflex t =>
  Inputs t ->
  Outputs t
ex05 (Inputs dMoney dCarrot dCelery dCucumber dSelected eBuy eRefund) =
  let
    stockSingleton :: Stock -> Map.Map Text Stock
    stockSingleton stock@(Stock prod _) = Map.singleton (pName prod) stock

    dProductStock :: Dynamic t (Map.Map Text Stock)
    dProductStock = mconcat $ fmap stockSingleton <$> [dCarrot, dCelery, dCucumber]

    eStockMaybe :: Event t (Maybe Stock)
    eStockMaybe = Map.lookup <$> current dSelected <@> updated dProductStock

    eStock :: Event t Stock
    eStock = fmapMaybe id eStockMaybe

    checkNotEnoughMoney money (Stock (Product _ cost) _) = money < cost
    checkNotEnoughStock (Stock _ 0) = True
    checkNotEnoughStock _ = False

    eError =
      leftmost [ ItemOutOfStock <$ ffilter checkNotEnoughStock eStock
               , NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> current dMoney <@> eStock)
               ]

    eVend =
      pName . sProduct <$> difference eStock eError

    eSpend =
      pCost . sProduct <$> difference eStock eError

    eChange =
      current dMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex05
#endif
