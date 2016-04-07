{-# LANGUAGE UndecidableInstances   #-}
-- * Descrizione della libreria

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module LibCTRex where

import           Data.Default     (Default (..))
import           Data.OpenRecords

newtype Loudness = Loudness Double
                 deriving (Eq, Show)

instance Default Loudness where
  def = Loudness 0

data Part = Soprano | Alto | Tenor | Basso deriving (Show)

instance Default Part where
  def = Soprano

data Pitch = C | D | E | F | G | A | B deriving (Show)

volume :: Label "volume"
volume = Label

pitch :: Label "pitch"
pitch = Label

part :: Label "part"
part = Label

type FugueNote = ("pitch" '::= Pitch :| "volume" '::= Loudness :| "part" '::= Part :| Empty)

note :: Rec FugueNote
note = (pitch := C .| volume := Loudness 3 .| part := Basso .| empty)

instance (Forall r Default) => Default (Rec r) where
  def = rinit (undefined :: CWit Default) def

-- Guardare la domanda sul repo git dell'autore:

-- c :: forall r. Forall (r :- "pitch") Default => Rec r
-- c = pitch := C .| (def :: Rec (r :- "pitch"))
