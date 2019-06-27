{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ex06.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex06.Common
import Ex06.Run

ex06 ::
  forall t m. ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex06 (Inputs dMoney dCarrot dCelery dCucumber dSelected eBuy eRefund) = do
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

  dVend :: Dynamic t Text <- holdDyn "" $ leftmost
    [ eVend
    , errorText <$> eError
    , "" <$ eRefund
    ]

  dChange :: Dynamic t Int <- holdDyn 0 $ leftmost
    [ eChange
    , 0 <$ eError
    , 0 <$ eBuy
    ]


  pure $ Outputs eVend eSpend eChange eError dChange dVend

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex06
#endif
