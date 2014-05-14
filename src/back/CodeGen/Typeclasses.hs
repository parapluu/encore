{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Typeclasses where

-- | implements translation of one representation to another.  We use
-- this to implement translation of AST nodes (see "AST") to CCode
-- (see "CCode.Main")
class Translatable a b where
  translate :: a -> b

-- | same same but different: the semantics of FwdDeclaration allows
-- to implement another translation, namely to a forward declaration.
-- Known instances are @FwdDeclaration ClassDecl (CCode Toplevel)@
-- ("CodeGen.ClassDecl") and @FwdDeclaration Program (CCode Toplevel)@
-- ("CodeGen.Program")
class FwdDeclaration a b where
  fwd_decls :: a -> b
