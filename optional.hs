module Optional where

import Data.Monoid

data Optional a = Nada
                | Only a
                  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada = Nada
  Nada <> Only a = Only a
  Only a <> Nada = Only a
  Only a <> Only b = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = 
  mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj <> " wife."]
