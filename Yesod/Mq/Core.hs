{-# LANGUAGE TemplateHaskell
           , QuasiQuotes
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , UndecidableInstances
           , TypeFamilies #-}

module Yesod.Mq.Core where

import Yesod
import Yesod.Handler
import Language.Haskell.TH.Syntax

data Mq = Mq
  deriving (Eq, Show, Read)

getMq :: a -> Mq
getMq = const Mq

mkYesodSub "Mq" 
    [ ClassP (mkName "YesodSubRoute") [ConT $ mkName "Mq", VarT $ mkName "master"]
    ] [$parseRoutes|
/js MqJsR GET
|]

getMqJsR :: GHandler Mq y RepPlain
getMqJsR = $(embedFile typeJavascript "static/mq-1.0.1.debug.js")

