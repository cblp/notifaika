{-
    Notifaika reposts notifications
    from different feeds to Gitter chats.
    Copyright (C) 2016 Alexander Vershilov

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE UndecidableInstances, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Gitter.Eff where

import Network.Gitter
import Network.Gitter.Monad

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.Lift

instance (SetMember Lift (Lift IO) r, Member (Reader Gitter) r) => MonadGitter (Eff r) where
    runGitterAction path apiRequest = do
      config <- ask
      lift $ runGitterT config $ runGitterAction path apiRequest

runGitterEff :: Gitter -> Eff (Reader Gitter :> r) a -> Eff r a
runGitterEff gitter action = runReader action gitter
