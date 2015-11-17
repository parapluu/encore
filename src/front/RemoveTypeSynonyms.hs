{-# OPTIONS_GHC -Wall #-}


module RemoveTypeSynonyms where

import AST.AST
import AST.Util
import Types

class RemoveTypeSynonyms t where
  removeTypeSynonyms :: t -> t

instance RemoveTypeSynonyms a => RemoveTypeSynonyms [a] where
  removeTypeSynonyms = map removeTypeSynonyms

instance RemoveTypeSynonyms Program where
  removeTypeSynonyms p@Program{imports, functions, traits, classes}
    = p{imports = removeTypeSynonyms imports
       ,functions = removeTypeSynonyms functions
       ,traits = removeTypeSynonyms traits
       ,classes = removeTypeSynonyms classes}

instance RemoveTypeSynonyms FunctionHeader where
  removeTypeSynonyms h
    = h{htype = removeTypeSynonyms (htype h)
       ,hparams = removeTypeSynonyms (hparams h)}

instance RemoveTypeSynonyms Function where
  removeTypeSynonyms f@Function{funheader, funbody}
    = f{funheader = removeTypeSynonyms funheader
       ,funbody = removeTypeSynonyms funbody}

instance RemoveTypeSynonyms ClassDecl where
  removeTypeSynonyms c@Class{ccapability, cfields, cmethods}
    = c{ccapability = removeTypeSynonyms ccapability
       ,cfields = removeTypeSynonyms cfields
       ,cmethods = removeTypeSynonyms cmethods}

instance RemoveTypeSynonyms TraitDecl where
  removeTypeSynonyms c@Trait{tmethods}
    = c{tmethods = removeTypeSynonyms tmethods}

instance RemoveTypeSynonyms ImportDecl where
  removeTypeSynonyms pim@PulledImport{iprogram} = pim{iprogram=removeTypeSynonyms iprogram}
  removeTypeSynonyms i = i

instance RemoveTypeSynonyms FieldDecl where
  removeTypeSynonyms f@Field{ftype} = f{ftype = removeTypeSynonyms ftype}

instance RemoveTypeSynonyms ParamDecl where
  removeTypeSynonyms p@Param{ptype} = p{ptype = removeTypeSynonyms ptype}

instance RemoveTypeSynonyms MethodDecl where
  removeTypeSynonyms m@Method{mheader, mbody}
    = m{mheader = removeTypeSynonyms mheader
       ,mbody = removeTypeSynonyms mbody}

updateMeta :: HasMeta a => a -> a
updateMeta e = setType (removeTypeSynonyms (getType e)) e

instance RemoveTypeSynonyms Expr where
  removeTypeSynonyms = extend (removeTypeSynonyms' . updateMeta)
   where
    removeTypeSynonyms' e
        | exposesType e = e{ty = removeTypeSynonyms (ty e)}
        | otherwise = e
    exposesType TypedExpr{}   = True
    exposesType ArrayNew{}    = True
    exposesType NewWithInit{} = True
    exposesType New{}         = True
    exposesType Peer{}        = True
    exposesType Embed{}       = True
    exposesType _             = False

instance RemoveTypeSynonyms Type where
  removeTypeSynonyms = unfoldTypeSynonyms
