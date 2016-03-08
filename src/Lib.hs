-- * Preamble

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import           Control.Lens         (makeLenses)
import           Data.Default         (Default (..))
import           Data.Proxy
import           Data.Singletons.TH   (genSingletons)
import           Data.Vinyl
import           Data.Vinyl.TypeLevel

-- * First Properties
-- Let's define some constructors for simple properties of the music we're
-- interested in:

newtype Loudness = Loudness Double
                 deriving (Eq, Show)

instance Default Loudness where
  def = Loudness 0

data Pitch = C | D | E | F | G | A | B deriving (Show)

-- * Vinyl Instances

-- Now, let's create some extensible records, using the vinyl library, based on
-- three proprieties of interest: the Volume and Pitch we saw before, and the
-- idea -- of Part, which is encoded as an integer (in a vocal fugue may be
-- Bass, Tenor etc)

data NoteProperty = Volume | Part | PitchValue deriving (Show, Ord, Eq)
genSingletons [''NoteProperty]

type FugueNote = Rec Attr ['PitchValue, 'Part, 'Volume]

type family Interpretation (np :: NoteProperty) :: * where
  Interpretation 'Volume     = Loudness
  Interpretation 'Part       = Int
  Interpretation 'PitchValue = Pitch

newtype Attr np = Attr { _unAttr :: Interpretation np }
makeLenses ''Attr

instance Show (Attr 'Volume)     where show (Attr x) = "volume: " ++ show x
instance Show (Attr 'Part)       where show (Attr x) = "part: " ++ show x
instance Show (Attr 'PitchValue) where show (Attr x) = "pitch: " ++ show x
instance Default (Interpretation a) => Default (Attr a) where def = Attr def

(=::) :: sing f -> Interpretation f -> Attr f
_ =:: x = Attr x

-- A simple note may be written in this setting as:
note :: FugueNote
note = (SPitchValue =:: C) :& (SPart =:: 1) :& (SVolume =:: Loudness 1) :& RNil

-- * The main problem

-- I'd like to be able to write "c" and mean the note which has pitch C and the
-- other fields as default ones, but without committing to a particular record
-- choice. So I begin defining a typeclass:

instance Default (Rec f '[]) where
  def = RNil

instance (Default (f r), Default (Rec f rs)) => Default (Rec f (r ': rs)) where
  def = def :& def

-- recDef :: RecAll f rs Default => Rec proxy rs -> Rec f rs
-- recDef RNil = RNil
-- recDef (_ :& rs) = def :& recDef rs

-- instance (RecAll f rs Default, RecApplicative rs) => Default (Rec f rs) where
--   def = recDef (rpure Proxy)

-- c, d, e, f, g, a, b :: (Default (Attr r), Default (Rec Attr rs)) =>  Rec Attr ('PitchValue ': r ': rs)
c, d, e, f, g, a, b :: (RecAll Attr rs Default) =>  Rec Attr ('PitchValue ': rs)
c = (SPitchValue =:: C) :& def
d = (SPitchValue =:: D) :& def
e = (SPitchValue =:: E) :& def
f = (SPitchValue =:: F) :& def
g = (SPitchValue =:: G) :& def
a = (SPitchValue =:: A) :& def
b = (SPitchValue =:: B) :& def

-- chord :: [FugueNote]
-- chord = [c,e,g]

