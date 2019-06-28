{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Ex09.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex09.Common
import Ex09.Run

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

ex09 ::
  forall t m. ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Event t Text)
ex09 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
  let
    stockSingleton :: Stock -> Map.Map Text Stock
    stockSingleton stock@(Stock prod _) = Map.singleton (pName prod) stock

    stocks1 :: [Dynamic t (Map.Map Text Stock)]
    stocks1 = (fmap stockSingleton) <$> [dCarrot, dCelery, dCucumber]

    stocks2 :: Dynamic t [(Map.Map Text Stock)]
    stocks2 = sequence stocks1

    stocks3 :: Dynamic t (Map.Map Text Stock)
    stocks3 = mconcat <$> stocks2

    dProductStock :: Dynamic t (Map.Map Text Stock)
    dProductStock = stocks3

    emStock :: Event t (Maybe Stock)
    emStock = Map.lookup <$> current dSelected <*> current dProductStock <@ eBuy

    eStockBuyIntent :: Event t Stock
    eStockBuyIntent = fmapMaybe id emStock

    checkNotEnoughStock (Stock _ 0) = True
    checkNotEnoughStock _ = False

    eItemOutOfStockErr :: Event t Error
    eItemOutOfStockErr = ItemOutOfStock <$ ffilter checkNotEnoughStock eStockBuyIntent

    checkNotEnoughMoney money (Stock (Product _ cost) _) = money < cost

    eNotEnoughMoneyErr :: Event t Error
    eNotEnoughMoneyErr = NotEnoughMoney <$ (ffilter id $ (checkNotEnoughMoney <$> current dMoney <@> eStockBuyIntent))

    eError :: Event t Error
    eError = leftmost [eItemOutOfStockErr, eNotEnoughMoneyErr]

    eStock :: Event t Stock
    eStock = difference eStockBuyIntent eError

    eVend :: Event t Text
    eVend = pName . sProduct <$> eStock

    eSpend :: Event t Money
    eSpend = pCost . sProduct <$> eStock

  dMoney <- dynMoney eAdd eSpend eRefund
  dChange <- dynChange (current dMoney <@ eRefund) (leftmost [eAdd, eBuy])

  eBuy <- buyRow
  eAdd <- moneyRow dMoney
  eRefund <- changeRow dChange

  dVend <- holdDyn "" $ leftmost [ eVend
                                 , errorText <$> eError
                                 , "" <$ eAdd
                                 , "" <$ eRefund
                                 ]

  trayRow dVend

  pure eVend

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host mkStock ex09
#endif

buyRow :: MonadWidget t m => m (Event t ())
buyRow = el "tr" $ do
  el "td" $ pure ()
  el "td" $ pure ()
  el "td" $ pure ()
  el "td" $ button "Buy"

moneyRow :: forall t m. MonadWidget t m => Dynamic t Money -> m (Event t ())
moneyRow dMoney = el "tr" $ do
  el "td" $ text "Money inserted:"
  el "td" $ pure ()
  el "td" $ dynText (moneyDisplay <$> dMoney)
  el "td" $ button "Add money"

changeRow :: forall t m. MonadWidget t m => Dynamic t Money -> m (Event t ())
changeRow dChange = el "tr" $ do
  el "td" $ text "Change:"
  el "td" $ pure ()
  el "td" $ dynText (moneyDisplay <$> dChange)
  el "td" $ button "Refund"

trayRow :: forall t m. MonadWidget t m => Dynamic t Text -> m ()
trayRow dVend = el "tr" $ do
  el "td" $ text "Tray:"
  el "td" $ pure ()
  el "td" $ dynText dVend
  el "td" $ pure ()

dynChange
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => Event t Money
  -> Event t ()
  -> m (Dynamic t Money)
dynChange eSet eReset = mdo
  dChange <- foldDyn ($) 0 $ leftmost
    [ const <$> eSet
    , const 0 <$ eReset
    ]
  pure dChange


dynMoney :: forall m t. (Reflex t, MonadHold t m, MonadFix m)
             => Event t ()
             -> Event t Money
             -> Event t ()
             -> m (Dynamic t Money)
dynMoney eAdd eSpend eRefund = mdo

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
