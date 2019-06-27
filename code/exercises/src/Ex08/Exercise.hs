{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ex08.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex08.Common
import Ex08.Run

mkStock ::
  forall m t. ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)
mkStock quantity product eVend = mdo
  let decStock :: Stock -> Stock
      decStock (Stock prod count) = Stock prod (count - 1)

  dStock <- foldDyn ($) (Stock product quantity) (decStock <$ eVend)

  let eSameProduct :: Event t Bool
      eSameProduct = (==) <$> (pName . sProduct <$> current dStock) <@> eVend

      eCantVend :: Event t Bool
      eCantVend = (==0) <$> (sQuantity <$> current dStock) <@ eVend

      eCanVend :: Event t ()
      eCanVend = () <$ (ffilter id $ difference eSameProduct eCantVend)

  pure dStock

ex08 ::
  forall t m. ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex08 (Inputs dCarrot dCelery dCucumber dSelected eAdd eBuy eRefund) = mdo
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
    host mkStock ex08
#endif

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
