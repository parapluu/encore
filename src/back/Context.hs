module Context (
  Context,
  mk,
  the_prog,
  the_class,
  the_method,
  the_locals,
  type_of,
  with_class,
  with_method,
  with_local,
  other_classes) where

import AST
import Data.Maybe

data Context = Context Program (Maybe (ClassDecl, Maybe (MethodDecl, [ParamDecl]))) deriving (Show)

mk :: Program -> Context
mk p = Context p Nothing

with_class :: ClassDecl -> Context -> Context
with_class c (Context p Nothing) = Context p (Just (c, Nothing))
with_class c (Context p (Just (_, m))) = Context p (Just (c, m))

with_method :: MethodDecl -> Context -> Context
with_method m (Context p (Just (c, _))) = Context p (Just (c, Just (m,[])))

with_local :: ParamDecl -> Context -> Context
with_local d (Context p (Just (c, Just (m, ds)))) = (Context p (Just (c, Just (m, d:ds))))
with_local d c = error $ "with_local: invalid input: " ++ show (d, c)

the_prog :: Context -> Program
the_prog (Context prog _) = prog

the_class :: Context -> Maybe ClassDecl
the_class (Context _ x) = x >>= Just . fst

the_method :: Context -> Maybe MethodDecl
the_method (Context _ x) = x >>= snd >>= return . fst

the_locals :: Context -> Maybe [ParamDecl]
the_locals (Context _ x) = x >>= snd >>= return . snd

type_of :: Name -> Context -> Maybe Type
type_of n c = do
  mthd <- the_method c
  locals <- the_locals c
  return $ head $ only_tys $ filter_name_matches n $ (mparams mthd ++ locals)
    where
      filter_name_matches :: Name -> [ParamDecl] -> [ParamDecl]
      filter_name_matches n = filter (\ (Param (_,n')) -> (n==n'))
      
      only_tys :: [ParamDecl] -> [Type]
      only_tys = map (\(Param (ty,_)) -> ty)

other_classes :: Context -> [ClassDecl]
other_classes ctx =
  let
    (Program clss) = (the_prog ctx)
    cls = (the_class ctx)
  in
   map fromJust $ filter (/= cls) (map Just clss)

