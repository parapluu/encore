{-|

The machinery used by "Typechecker.Typechecker" for handling
errors and backtracing.

-}

module Typechecker.TypeError (Backtrace
                             ,emptyBT
                             ,Pushable(push)
                             ,TCError(TCError)
                             ,Error(..)
                             ,TCWarning(TCWarning)
                             ,Warning(..)
                             ,currentMethodFromBacktrace) where

import Text.PrettyPrint
import Text.Parsec(SourcePos)
import Data.Maybe
import Data.List
import Text.Printf (printf)

import Identifiers
import Types
import AST.AST
import AST.PrettyPrinter

data BacktraceNode = BTFunction Path Type
                   | BTTrait Type
                   | BTClass Type
                   | BTParam ParamDecl
                   | BTField FieldDecl
                   | BTMethod MethodDecl
                   | BTRequirement Requirement
                   | BTExpr Expr
                   | BTTypedef Type
                   | BTBundle Path


instance Show BacktraceNode where
  show (BTFunction n ty) =
    concat ["In function '", show n, "' of type '", show ty, "'"]
  show (BTClass ty) = concat ["In class '", show ty, "'"]
  show (BTTrait ty) = concat ["In trait '", show ty, "'"]
  show (BTParam p) = concat ["In parameter '", show (ppParamDecl p), "'"]
  show (BTField f) =  concat ["In field '", show (ppFieldDecl f), "'"]
  show (BTMethod m) =
      let name = hname $ mheader m
          ty   = htype $ mheader m
          method | isStreamMethod m = "stream method"
                 | otherwise = "method"
      in
        concat ["In ", method, " '", show name, "' of type '", show ty, "'"]
  show (BTRequirement req)
      | isRequiredField req =
          concat ["In required field '", show . ppFieldDecl . rfield $ req, "'"]
      | isRequiredMethod req =
          concat ["In required method '"
                 ,show . ppFunctionHeader . rheader $ req
                 , "'"]
  show (BTExpr expr)
    | (isNothing . getSugared) expr = ""
    | otherwise =
      let str = show $ nest 2 $ ppSugared expr
      in "In expression: \n" ++ str
  show (BTTypedef tl) =
     concat ["In typedef '", show tl, "'"]
  show (BTBundle (Path [])) = ""
  show (BTBundle path) = concat ["In bundle '", show path, "'"]

type Backtrace = [(SourcePos, BacktraceNode)]
emptyBT :: Backtrace
emptyBT = []

currentMethodFromBacktrace :: Backtrace -> Maybe MethodDecl
currentMethodFromBacktrace [] = Nothing
currentMethodFromBacktrace ((_, BTExpr Closure{}):_) = Nothing
currentMethodFromBacktrace ((_, BTExpr Async{}):_) = Nothing
currentMethodFromBacktrace ((_, BTMethod m):_) = Just m
currentMethodFromBacktrace (_:bt) = currentMethodFromBacktrace bt

-- | A type class for unifying the syntactic elements that can be pushed to the
-- backtrace stack.

class Pushable a where
    push :: a -> Backtrace -> Backtrace
    pushMeta ::  HasMeta a => a -> BacktraceNode -> Backtrace -> Backtrace
    pushMeta m n bt = (getPos m, n) : bt

instance Pushable Program where
  push prog =
     pushMeta prog (BTBundle $ bundleName prog)

instance Pushable Function where
  push fun =
    pushMeta fun (BTFunction (functionName fun) (functionType fun))

instance Pushable TraitDecl where
  push t = pushMeta t (BTTrait (tname t))

instance Pushable ClassDecl where
    push c = pushMeta c (BTClass (cname c))

instance Pushable FieldDecl where
    push f = pushMeta f (BTField f)

instance Pushable ParamDecl where
    push p = pushMeta p (BTParam p)

instance Pushable MethodDecl where
    push m = pushMeta m (BTMethod m)

instance Pushable Requirement where
    push m = pushMeta m (BTRequirement m)

instance Pushable Expr where
    push expr = pushMeta expr (BTExpr expr)

instance Pushable Typedef where
    push t@(Typedef {typedefdef}) = pushMeta t (BTTypedef typedefdef)

refTypeName :: Type -> String
refTypeName ty
    | isClassType ty = "class '" ++ show (unsafeBase 13 (getId ty)) ++ "'"
    | isTraitType ty = "trait '" ++ show (unsafeBase 14 (getId ty)) ++ "'"
    | isCapabilityType ty = "capability '" ++ show ty ++ "'"
    | isUnionType ty = "union '" ++ show ty ++ "'"
    | otherwise = error $ "Util.hs: No refTypeName for " ++
                          Types.showWithKind ty

-- | The data type for a type checking error. Showing it will
-- produce an error message and print the backtrace.
data TCError = TCError Error Backtrace
instance Show TCError where
    show (TCError err []) =
        " *** Error during typechecking *** \n" ++
        show err ++ "\n"
    show (TCError err bt@((pos, _):_)) =
        " *** Error during typechecking *** \n" ++
        show pos ++ "\n" ++
        show err ++ "\n" ++
        concatMap showBT bt
        where
          showBT (_, node) =
              case show node of
                "" -> ""
                s  -> s ++ "\n"

data Error =
    DistinctTypeParametersError Type
  | WrongNumberOfMethodArgumentsError Name Type Int Int
  | WrongNumberOfFunctionArgumentsError Path Int Int
  | WrongNumberOfTypeParametersError Type Int Type Int
  | MissingFieldRequirementError FieldDecl Type
  | CovarianceViolationError FieldDecl Type Type
  | RequiredFieldMismatchError FieldDecl Type Type Bool
  | NonDisjointConjunctionError Type Type FieldDecl
  | OverriddenMethodError Name Type
  | IncludedMethodConflictError Name Type Type
  | MissingMethodRequirementError (FunctionHeader Name) Type
  | MissingMainClass
  | UnknownTraitError Type
  | UnknownRefTypeError Type
  | MalformedCapabilityError Type
  | RecursiveTypesynonymError Type
  | DuplicateThingError String String
  | PassiveStreamingMethodError
  | StreamingConstructorError
  | MainMethodArgumentsError
  | FieldNotFoundError Name Type
  | MethodNotFoundError Name Type
  | TraitsInActiveClassError
  | NonCallableTargetError Type
  | NonSendableTargetError Type
  | MainMethodCallError
  | ConstructorCallError
  | ExpectingOtherTypeError String Type
  | NonStreamingContextError Expr
  | UnboundFunctionError Path
  | NonFunctionTypeError Type
  | BottomTypeInferenceError
  | IfInferenceError
  | IfBranchMismatchError Type Type
  | EmptyMatchClauseError
  | ActiveMatchError
  | MatchInferenceError
  | ThisReassignmentError
  | PatternTypeMismatchError Expr Type
  | NonMaybeExtractorPatternError Expr
  | InvalidPatternError Expr
  | CannotReadFieldError Expr
  | NonAssignableLHSError
  | ValFieldAssignmentError Name Type
  | UnboundVariableError Name
  | ObjectCreationError Type
  | NonIterableError Type
  | EmptyArrayLiteralError
  | NonIndexableError Type
  | NonSizeableError Type
  | FormatStringLiteralError
  | UnprintableExpressionError Type
  | WrongNumberOfPrintArgumentsError Int Int
  | UnaryOperandMismatchError UnaryOp Type
  | BinaryOperandMismatchError BinaryOp String Type Type
  | UndefinedBinaryOperatorError BinaryOp
  | NullTypeInferenceError
  | CannotBeNullError Type
  | TypeMismatchError Type Type
  | TypeWithCapabilityMismatchError Type Type Type
  | TypeVariableAmbiguityError Type Type Type
  | FreeTypeVariableError Type
  | UnionMethodAmbiguityError Type Name
  | MalformedUnionTypeError Type Type
  | AmbiguousLookup Type [Path]
  | AmbiguousVariableLookup Name [Path]
  | AmbiguousFunctionLookup Path [Path]
  | ImportedNamesNotFound [Name]
  | HiddenNamesNotFound [Name]
  | NamesBothImportedAndHidden [Name]
  | ExportedNamesLackingBinding [Name]
  | SimpleError String

arguments 1 = "argument"
arguments _ = "arguments"

instance Show Error where
    show (DistinctTypeParametersError ty) =
        printf "Type parameters of '%s' must be distinct" (show ty)
    show (WrongNumberOfMethodArgumentsError name targetType expected actual) =
        let nameWithKind =
              (if name == Name "_init"
               then "Constructor"
               else "Method '" ++ show name ++ "'") ++
               " in " ++ refTypeName targetType
        in printf "%s expects %d %s. Got %d"
           nameWithKind expected (arguments expected) actual
    show (WrongNumberOfFunctionArgumentsError name expected actual) =
        printf "Function %s expects %d %s. Got %d"
               (show name) expected (arguments expected) actual
    show (WrongNumberOfTypeParametersError ty1 n1 ty2 n2) =
        printf "'%s' expects %d type %s, but '%s' has %d"
              (show ty1) n1 (arguments n1) (show ty2) n2
    show (MissingFieldRequirementError field trait) =
        printf "Cannot find field '%s' required by included %s"
               (show field) (refTypeName trait)
    show (CovarianceViolationError field expected trait) =
        printf ("Field '%s' must have a subtype of '%s' to meet " ++
                "the requirements of included %s")
               (show field) (show expected) (refTypeName trait)
    show (RequiredFieldMismatchError field expected trait isSub) =
        printf ("Field '%s' must exactly match type '%s' " ++
                "to meet the requirements of included %s%s")
               (show field) (show expected) (refTypeName trait)
               (if isSub
                then ". Consider turning '" ++ show (fname field) ++
                     "' into a val-field in " ++ refTypeName trait
                else "")
    show (NonDisjointConjunctionError left right field) =
        printf
          "Conjunctive traits '%s' and '%s' cannot share mutable field '%s'"
           (show left) (show right) (show field)
    show (OverriddenMethodError name trait) =
        printf "Method '%s' is defined both in current class and %s"
               (show name) (refTypeName trait)
    show (IncludedMethodConflictError name left right) =
        printf "Conflicting inclusion of method '%s' from %s and %s"
               (show name) (refTypeName left) (refTypeName right)
    show (MissingMethodRequirementError header trait) =
        printf "Cannot find method '%s' required by included %s"
               (show $ ppFunctionHeader header) (refTypeName trait)
    show (UnknownTraitError ty) =
        printf "Couldn't find trait '%s'" (show (getId ty))
    show MissingMainClass = "Couldn't find active class 'Main'"
    show (UnknownRefTypeError ty) =
        printf "Couldn't find class, trait or typedef '%s'" (show ty)
    show (MalformedCapabilityError ty) =
        printf "Cannot form capability with %s" (Types.showWithKind ty)
    show (RecursiveTypesynonymError ty) =
        printf "Type synonyms cannot be recursive. One of the culprits is %s"
               (show (getId ty))
    show (DuplicateThingError kind thing) =
        printf "Duplicate %s of %s" kind thing
    show PassiveStreamingMethodError =
        "Cannot have streaming methods in a passive class"
    show StreamingConstructorError =
        "Constructor cannot be streaming"
    show MainMethodArgumentsError =
        "Main method must have argument type () or ([String])"
    show (FieldNotFoundError name ty) =
        printf "No field '%s' in %s"
               (show name) (refTypeName ty)
    show (MethodNotFoundError name ty) =
        let nameWithKind = if name == Name "_init"
                           then "constructor"
                           else "method '" ++ show name ++ "'"
            targetType = if isRefType ty
                         then refTypeName ty
                         else Types.showWithKind ty
        in printf "No %s in %s"
                  nameWithKind targetType
    show (TraitsInActiveClassError) =
        "Traits can only be used for passive classes"
    show (NonCallableTargetError targetType) =
        printf "Cannot call method on expression of type '%s'"
               (show targetType)
    show (NonSendableTargetError targetType) =
        printf "Cannot send message to expression of type '%s'"
               (show targetType)
    show MainMethodCallError = "Cannot call the main method"
    show ConstructorCallError =
        "Constructor method 'init' can only be called during object creation"
    show (ExpectingOtherTypeError something ty) =
        printf "Expected %s but found expression of type '%s'"
               something (show ty)
    show (NonStreamingContextError e) =
        printf "Cannot have '%s' outside of a streaming method"
               (show $ ppSugared e)
    show (UnboundFunctionError name) =
        printf "Unbound function or variable '%s'" (show name)
    show (NonFunctionTypeError ty) =
        printf "Cannot use value of type '%s' as a function" (show ty)
    show BottomTypeInferenceError = "Cannot infer type of 'Nothing'"
    show IfInferenceError = "Cannot infer result type of if-statement"
    show (IfBranchMismatchError ty1 ty2) =
        "Type mismatch in different branches of if-statement:\n" ++
        "  then:  " ++ show ty1 ++ "\n" ++
        "  else:  " ++ show ty2
    show EmptyMatchClauseError = "Match statement must have at least one clause"
    show ActiveMatchError = "Cannot match on an active object"
    show MatchInferenceError = "Cannot infer result type of match expression"
    show ThisReassignmentError = "Cannot rebind variable 'this'"
    show (PatternTypeMismatchError pattern ty) =
        printf "Pattern '%s' does not match expected type '%s'"
               (show $ ppSugared pattern) (show ty)
    show (NonMaybeExtractorPatternError pattern) =
        printf "Extractor '%s' must return a Maybe type to be used as a pattern"
               (show $ ppSugared pattern)
    show (InvalidPatternError pattern) =
        printf "'%s' is not a valid pattern"
               (show $ ppSugared pattern)
    show (CannotReadFieldError target) =
        printf "Cannot read field of expression '%s' of %s"
          (show $ ppSugared target) (Types.showWithKind $ getType target)
    show NonAssignableLHSError =
        "Left-hand side cannot be assigned to"
    show (ValFieldAssignmentError name targetType) =
        printf "Cannot assign to val-field '%s' in %s"
               (show name) (refTypeName targetType)
    show (UnboundVariableError name) =
        printf "Unbound variable '%s'" (show name)
    show (ObjectCreationError ty)
        | isMainType ty = "Cannot create additional Main objects"
        | isCapabilityType ty =
            printf "Cannot create instance of %s (type must be a class)"
                   (refTypeName ty)
        | otherwise = printf "Cannot create object of type '%s'" (show ty)
    show (NonIterableError ty) =
        printf "Type '%s' is not iterable" (show ty)
    show EmptyArrayLiteralError = "Array literal must have at least one element"
    show (NonIndexableError ty) =
        printf "Type '%s' is not indexable" (show ty)
    show (NonSizeableError ty) =
        printf "Type '%s' has no size" (show ty)
    show FormatStringLiteralError =
        "Formatted printing expects first argument to be a string literal"
    show (UnprintableExpressionError ty) =
        printf "Expression of type '%s' is not printable" (show ty)
    show (WrongNumberOfPrintArgumentsError expected actual) =
        printf ("Wrong number of arguments to print. Format string " ++
                "expects %d %s. Found %d") expected (arguments expected) actual
    show (UnaryOperandMismatchError op ty) =
        printf "Operator '%s' is not defined for values of type '%s'"
               (show op) (show ty)
    show (BinaryOperandMismatchError op kind lType rType) =
        printf ("Operator '%s' is only defined for %s types\n" ++
                "   Left type: %s\n" ++
                "   Right type: %s")
               (show op) kind (show lType) (show rType)
    show (UndefinedBinaryOperatorError op) =
        printf "Undefined binary operator '%s'" (show op)
    show NullTypeInferenceError =
        "Cannot infer type of null valued expression. " ++
        "Try adding type annotations"
    show (CannotBeNullError ty) =
        printf ("Null valued expression cannot have type '%s' " ++
                "(must have reference type)") (show ty)
    show (TypeMismatchError actual expected) =
        printf "Type '%s' does not match expected type '%s'"
               (show actual) (show expected)
    show (TypeWithCapabilityMismatchError actual cap expected) =
        printf "Type '%s' with capability '%s' does not match expected type '%s'"
               (show actual) (show cap) (show expected)
    show (TypeVariableAmbiguityError expected ty1 ty2) =
        printf "Type variable '%s' cannot be bound to both '%s' and '%s'"
               (show expected) (show ty1) (show ty2)
    show (FreeTypeVariableError ty) =
        printf "Type variable '%s' is unbound" (show ty)
    show (UnionMethodAmbiguityError ty name) =
        printf "Cannot disambiguate method '%s' in %s"
               (show name) (Types.showWithKind ty)
    show (MalformedUnionTypeError ty union) =
        printf "Type '%s' is not compatible with %s"
               (show ty) (Types.showWithKind union)
    show (AmbiguousLookup ty paths) =  
        printf "Ambiguous lookup of class, trait or typedef '%s'.\nFound in several modules:\n %s\n" (show ty)
          (concat (map (\a -> "    " ++ show a ++ "\n") paths))
    show (AmbiguousVariableLookup ty paths) =
        printf "Ambiguous lookup of variable '%s'.\nFound in several modules:\n %s\n" (show ty)
          (concat (map (\a -> "    " ++ show a ++ "\n") paths))
    show (AmbiguousFunctionLookup ty paths) =
        printf "Ambiguous lookup of function '%s'.\nFound in several modules:\n %s\n" (show ty)
          (concat (map (\a -> "    " ++ show a ++ "\n") paths))
    show (ImportedNamesNotFound names) =
        printf "Imported names not found: %s." (intercalate ", " $ map show names)
    show (HiddenNamesNotFound names) =
        printf "Hidden names not found: %s." (intercalate ", " $ map show names)
    show (NamesBothImportedAndHidden names) =
        printf "Imported names not found: %s." (intercalate ", " $ map show names)
    show (ExportedNamesLackingBinding names) =
        printf "Exported names have no binding: %s." (intercalate ", " $ map show names)
    show (SimpleError msg) = msg


data TCWarning = TCWarning Backtrace Warning
instance Show TCWarning where
    show (TCWarning [] w) =
        "Warning:\n" ++
        show w
    show (TCWarning ((pos, _):_) w) =
        "Warning at " ++ show pos ++ ":\n" ++
        show w

data Warning = StringDeprecatedWarning
             | StringIdentityWarning
instance Show Warning where
    show StringDeprecatedWarning =
        "Type 'string' is deprecated. Use 'String' instead."
    show StringIdentityWarning =
        "Comparing String identity. Equality should be compared using 'equals'"
