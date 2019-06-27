{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex01.Exercise where

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex01.Common
import Ex01.Run

ex01 ::
  Reflex t =>
  Int ->
  Inputs t ->
  Outputs t
ex01 money (Inputs eCarrot eCelery eCucumber eRefund) =
  let
    eProduct = leftmost [carrot <$ eCarrot, celery <$ eCelery, cucumber <$ eCucumber]
    eProdWithEnoughMoney = ffilter (\(Product _ cost) -> cost <= money) eProduct
    eNotEnoughMoney = () <$ ffilter (> money) (pCost <$> eProduct)
    eVend =
      leftmost ["Insufficient funds" <$ eNotEnoughMoney, pName <$> eProdWithEnoughMoney]
    eSpend =
      pCost <$> eProdWithEnoughMoney
    eChange =
      money <$ eRefund
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex01
#endif
