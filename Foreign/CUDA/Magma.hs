{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.Magma where

import Foreign.CUDA.Magma.TH
import Foreign.CUDA.Magma.Types
import Foreign.C.Types

foreign import ccall safe "static /usr/local/magma/include/magma.h magma_init"
  initialize :: IO ()

foreign import ccall safe "static /usr/local/magma/include/magma.h magma_finalize"
  finalize :: IO ()

$(doIO $ makeFFIDecs "magma" magmaFile)
$(doIO $ makeAllFuncs "magma" magmaFile)
$(doIO $ makeClassDecs "magma" magmaFile)
