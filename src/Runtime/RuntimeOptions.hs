{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.RuntimeOptions where

import           Control.Lens

data RuntimeOptions = RuntimeOptions
  { _runtimeOptionsVerbose :: Bool
  } deriving (Show, Eq)

makeFields ''RuntimeOptions
