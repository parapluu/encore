{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module CodeGen.Typeclasses where

-- | Implements translation of one representation to another. We
-- use this to implement translation of AST nodes (see "AST") to
-- CCode (see "CCode.Main")
class Translatable a b | a -> b where
  translate :: a -> b