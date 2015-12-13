module Exceptions where

import Control.Exception
import Data.Typeable

data HalcException =
  NoVarException String
  | ParseError String
--  | AlexError String
  deriving (Show, Typeable)

instance Exception HalcException
