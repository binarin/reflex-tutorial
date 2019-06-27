{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex02.Exercise where

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex02.Common
import Ex02.Run

ex02 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex02 (Inputs bMoney eCarrot eCelery eCucumber eRefund) =
  let
    eProduct = leftmost [carrot <$ eCarrot, celery <$ eCelery, cucumber <$ eCucumber]
    eNotEnoughMoney = attachWithMaybe (\money cost -> if cost > money then Just () else Nothing) bMoney (pCost <$> eProduct)

    eSale = difference eProduct eNotEnoughMoney

    eVend =
      leftmost ["Insufficient funds" <$ eNotEnoughMoney, pName <$> eSale]
    eSpend =
      pCost <$> eSale
    eChange =
      bMoney <@ eRefund
    eError =
      NotEnoughMoney <$ eNotEnoughMoney


  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex02
#endif
