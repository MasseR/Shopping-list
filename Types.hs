module Types where

import Network.CGI (CGIT)
import Control.Monad.State (StateT)
import Data.ShoppingList (ShoppingList)

type Shop a = CGIT (StateT ShoppingList IO) a

