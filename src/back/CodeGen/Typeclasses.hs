{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Typeclasses where

class Translatable a b where
  translate :: a -> b

class FwdDeclaration a b where
  fwd_decls :: a -> b
