{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ex07.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex07.Common
import Ex07.Run


moneyTracker :: forall m t. (Reflex t, MonadHold t m, MonadFix m)
             => Event t ()
             -> Event t Money
             -> Event t ()
             -> m (Dynamic t Money)
moneyTracker eAdd eSpend eRefund = mdo

  let decreaseIfEnough :: Money -> Money -> Maybe Money
      decreaseIfEnough has need
        | has >= need = Just (has - need)
        | otherwise = Nothing

  dMoney <- foldDyn ($) 0 $ leftmost [ (+1) <$ eAdd
                                     , eDecrease
                                     , const 0 <$ eRefund
                                     ]

  let eMaybeDecrease :: Event t (Maybe Money)
      eMaybeDecrease = decreaseIfEnough <$> current dMoney <@> eSpend

      eDecrease :: Event t (Money -> Money)
      eDecrease = const <$> fmapMaybe id eMaybeDecrease

  pure dMoney


ex07 ::
  forall m t. ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex07 (Inputs dCarrot dCelery dCucumber dSelected eAdd eBuy eRefund) = mdo
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

  dMoney <- moneyTracker eAdd eSpend eRefund

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

  pure $ Outputs eVend eSpend eChange eError dMoney dChange dVend

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex07
#endif
