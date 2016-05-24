{-|

The machinery used by "Typechecker.Typechecker" and
"Typechecker.Capturechecker" for handling errors and backtracing.

-}

module Typechecker.TypeError (Backtrace
                             ,emptyBT
                             ,Pushable(push)
                             ,TCError(TCError)
                             ,Error(..)
                             ,TCWarning(TCWarning)
                             ,Warning(..)
                             ,CCError(CCError)
                             ,returnTypeBT
                             ,currentMethodFromBacktrace
                             ,loopInBacktrace
                             ,safeToSpeculateBT
                             ,safeToReadOnceBT
                             ) where

import Text.PrettyPrint
import Text.Parsec(SourcePos)
import Data.Maybe
import Data.List
import Text.Printf (printf)

import Identifiers
import Types
import AST.AST
import AST.PrettyPrinter

data BacktraceNode = BTFunction Name Type
                   | BTTrait Type
                   | BTClass Type
                   | BTParam ParamDecl
                   | BTField FieldDecl
                   | BTMethod MethodDecl
                   | BTRequirement Requirement
                   | BTExpr Expr
                   | BTTypedef Type


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

type Backtrace = [(SourcePos, BacktraceNode)]
emptyBT :: Backtrace
emptyBT = []

returnTypeBT :: Backtrace -> Type
returnTypeBT [] = error "TypeError.hs: Tried to get the return type when not in a method or function"
returnTypeBT ((_, BTMethod m):_) = methodType m
returnTypeBT ((_, BTFunction _ ftype):_) = ftype
returnTypeBT (_:bt) = returnTypeBT bt

currentMethodFromBacktrace :: Backtrace -> Maybe MethodDecl
currentMethodFromBacktrace [] = Nothing
currentMethodFromBacktrace ((_, BTExpr Closure{}):_) = Nothing
currentMethodFromBacktrace ((_, BTExpr Async{}):_) = Nothing
currentMethodFromBacktrace ((_, BTMethod m):_) = Just m
currentMethodFromBacktrace (_:bt) = currentMethodFromBacktrace bt

loopInBacktrace :: Backtrace -> Bool
loopInBacktrace = any (isLoopHead . snd)
    where
      isLoopHead (BTExpr For{}) = True
      isLoopHead (BTExpr While{}) = True
      isLoopHead (BTExpr Foreach{}) = True
      isLoopHead (BTExpr Repeat{}) = True
      isLoopHead _ = False

safeToSpeculateBT :: Backtrace -> Bool
safeToSpeculateBT ((_, current):(_, parent):_)
    | BTExpr Speculate{} <- parent = True
    | BTExpr CAT{}       <- parent = True
    | BTExpr Freeze{}    <- parent = True
    | BTExpr IsFrozen{}  <- parent = True
    | BTExpr Binop{}     <- parent = True
    | BTExpr Unary{}     <- parent = True
    | BTExpr e@FieldAccess{} <- current
    , BTExpr Assign{lhs}     <- parent
      = e == lhs
    | otherwise = False

safeToReadOnceBT :: Backtrace -> Bool
safeToReadOnceBT ((_, current):(_, parent):_)
    | BTExpr CAT{}           <- parent = True
    | BTExpr TryAssign{}     <- parent = True
    | BTExpr IsFrozen{}      <- parent = True
    | BTExpr e@FieldAccess{} <- current
    , BTExpr Assign{lhs}     <- parent
      = e == lhs
    | otherwise = False

-- | A type class for unifying the syntactic elements that can be pushed to the
-- backtrace stack.
class Pushable a where
    push :: a -> Backtrace -> Backtrace
    pushMeta ::  HasMeta a => a -> BacktraceNode -> Backtrace -> Backtrace
    pushMeta m n bt = (getPos m, n) : bt

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
    | isClassType ty = "class '" ++ getId ty ++ "'"
    | isTraitType ty = "trait '" ++ getId ty ++ "'"
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
  | WrongNumberOfFunctionArgumentsError Name Int Int
  | WrongNumberOfFunctionTypeArgumentsError Name Int Int
  | WrongNumberOfTypeParametersError Type Int Type Int
  | MissingFieldRequirementError FieldDecl Type
  | CovarianceViolationError FieldDecl Type Type
  | RequiredFieldMismatchError FieldDecl Type Type Bool
  | NonDisjointConjunctionError Type Type FieldDecl
  | OverriddenMethodError Name Type
  | IncludedMethodConflictError Name Type Type
  | MissingMethodRequirementError FunctionHeader Type
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
  | UnboundFunctionError Name
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
  | SpecFieldAssignmentError Name Type
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
  | ConcreteTypeParameterError Type
  | TypeArgumentInferenceError Name Type
  | CannotHaveModeError Type
  | ModelessError Type
  | ModeOverrideError Type
  | CannotConsumeError Expr
  | CannotGiveReadModeError
  | NonValInReadTraitError
  | NonSafeInReadTraitError Type
  | SubordinateReturnError Name
  | SubordinateArgumentError Expr
  | SubordinateFieldError Name
  | ThreadFieldError
  | ThreadReturnError Name
  | ThreadArgumentError Expr
  | MalformedConjunctionError Type Type Type
  | CannotUnpackError Type
  | CannotInferUnpackingError Type
  | NoLoopToBreakError
  | NonSpeculatableFieldError FieldDecl
  | CannotHaveRestrictedFieldsError Type
  | RestrictedFieldLookupError Name Type
  | NonSpeculatableTargetError
  | NonStableCatError Name
  | MalformedCatError
  | NonSpecFreezeError FieldDecl
  | NonFreezableFieldError Type
  | MalformedFreezeError
  | MalformedIsFrozenError
  | ModifierMismatchError FieldDecl FieldDecl Type
  | MissingSpeculationError FieldDecl
  | SpeculativeCatError Expr
  | OnceFieldTypeError Type
  | TryAssignError FieldDecl
  | MalformedTryAssignError
  | NonStableFieldAccessError FieldDecl
  | StrongRestrictionViolationError Name Expr Expr
  | ResidualAliasingError Name Type
  | NonSpineCatTargetError Expr
  | SimpleError String

arguments 1 = "argument"
arguments _ = "arguments"

typeParameters 1 = "type parameter"
typeParameters _ = "type parameters"

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
    show (WrongNumberOfFunctionTypeArgumentsError name expected actual) =
        printf "Function %s expects %d %s. Got %d"
               (show name) expected (typeParameters expected) actual
    show (WrongNumberOfTypeParametersError ty1 n1 ty2 n2) =
        printf "'%s' expects %d type %s, but '%s' has %d"
              (showWithoutMode ty1) n1 (arguments n1) (showWithoutMode ty2) n2
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
        printf "Couldn't find trait '%s'" (getId ty)
    show MissingMainClass = "Couldn't find active class 'Main'"
    show (UnknownRefTypeError ty) =
        printf "Couldn't find class, trait or typedef '%s'" (show ty)
    show (MalformedCapabilityError ty) =
        printf "Cannot form capability with %s" (Types.showWithKind ty)
    show (RecursiveTypesynonymError ty) =
        printf "Type synonyms cannot be recursive. One of the culprits is %s"
               (getId ty)
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
        printf "Unbound function variable '%s'" (show name)
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
    show (SpecFieldAssignmentError name targetType) =
        printf ("Cannot assign to spec-field '%s' " ++
                "in non-pristine %s without a CAT")
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
    show (ConcreteTypeParameterError ty) =
        printf "Concrete type '%s' cannot be used as a type parameter"
               (show ty)
    show (TypeArgumentInferenceError fn param) =
        printf "Cannot infer the type of parameter '%s' of function '%s'"
               (show param) (show fn)
    show (CannotHaveModeError ty) =
        printf "Cannot give mode to %s" (Types.showWithKind ty)
    show (ModelessError ty) =
        printf "No mode given to %s" (refTypeName ty)
    show (ModeOverrideError ty) =
        printf "Cannot override mode of %s" (Types.showWithKind ty)
    show (CannotConsumeError expr) =
        printf "Cannot consume '%s' of %s"
               (show $ ppSugared expr) (Types.showWithKind $ getType expr)
    show CannotGiveReadModeError =
        "Read mode must be manifestly set"
    show NonValInReadTraitError =
        "Read traits can only have val fields"
    show (NonSafeInReadTraitError ty) =
        printf "Read trait can not have field of non-safe type '%s'"
               (show ty)
    show (SubordinateReturnError name) =
        printf ("Method '%s' returns a subordinate capability and cannot " ++
                "be called from outside of its aggregate")
               (show name)
    show (SubordinateArgumentError arg) =
        printf ("Cannot pass subordinate argument '%s' " ++
                "outside of its aggregate")
               (show (ppSugared arg))
    show (SubordinateFieldError name) =
        printf ("Field '%s' is subordinate and cannot be accessed " ++
                "from outside of its aggregate")
               (show name)
    show ThreadFieldError =
        "Traits must have manifest thread mode to have thread fields"
    show (ThreadArgumentError arg) =
        printf ("Cannot pass thread local argument '%s' " ++
                "to another active object")
               (show (ppSugared arg))
    show (ThreadReturnError name) =
        printf ("Method '%s' returns a thread local capability and cannot " ++
                "be called by a different active object")
               (show name)
    show (MalformedConjunctionError ty nonDisjoint source) =
        printf "Type '%s' does not form a conjunction with '%s' in %s"
               (show ty) (show nonDisjoint) (Types.showWithKind source)
    show (CannotUnpackError source) =
        printf "Cannot unpack empty capability of class '%s'"
               (show source)
    show (CannotInferUnpackingError cap) =
        printf ("Unpacking of %s cannot be inferred. " ++
                "Try adding type annotations")
               (Types.showWithKind cap)
    show NoLoopToBreakError =
        "No loop to break from"
    show (NonSpeculatableFieldError fdecl) =
        printf "Field '%s' is not speculatable"
               (show fdecl)
    show (CannotHaveRestrictedFieldsError ty) =
        printf "Cannot have restricted fields in %s"
               (Types.showWithKind ty)
    show (RestrictedFieldLookupError f ty) =
        printf "Field '%s' is restricted in type '%s'"
               (show f) (show ty)
    show NonSpeculatableTargetError =
        "Can only speculate on field accesses"
    show (NonStableCatError f) =
        printf "Field '%s' must be stable to be used in a CAT"
               (show f)
    show MalformedCatError =
        "CAT must take one of the following forms:\n" ++
        "  CAT(x.f, y, y.g)" ++
        "  CAT(x.f, y.g, y)" ++
        "  CAT(x.f, y, z)"
    show (NonSpecFreezeError fdecl) =
        printf "Field '%s' is not freezable"
               (show fdecl)
    show (NonFreezableFieldError ty) =
        printf "Field of %s cannot be frozen"
               (Types.showWithKind ty)
    show MalformedFreezeError =
        "First argument of freeze must be a field access"
    show MalformedIsFrozenError =
        "First argument of isFrozen must be a field access"
    show (ModifierMismatchError cField expField trait) =
        printf ("Modifier of field '%s' is incompatible with " ++
                "modifier of '%s' required by %s")
               (show cField) (show expField) (refTypeName trait)
    show (MissingSpeculationError fdecl) =
        printf "Cannot read field '%s' without speculation"
               (show fdecl)
    show (SpeculativeCatError expr) =
        printf ("Cannot transfer ownership in '%s' of type '%s' " ++
                "since it contains speculative values")
               (show (ppSugared expr)) (show (getType expr))
    show (OnceFieldTypeError ty) =
        printf "Once-fields cannot have %s (must have ref-type)"
               (Types.showWithKind ty)
    show (TryAssignError fdecl) =
        printf "Field '%s' is not a once-field"
               (show fdecl)
    show MalformedTryAssignError =
        "Writing to a once-field must have the shape try(x.f = y)"
    show (NonStableFieldAccessError fdecl) =
        printf "Cannot read field '%s' without controlling its stability first"
               (show fdecl)
    show (StrongRestrictionViolationError f target arg) =
        printf ("Field '%s' is strongly restricted in the type of '%s' " ++
                "and must be unrestricted in the type of '%s'")
               (show f) (show (ppSugared target)) (show (ppSugared arg))
    show (ResidualAliasingError f ty) =
        printf ("Cannot create residual alias. Field '%s' " ++
                "is not strongly restricted in type '%s'")
               (show f) (show ty)
    show (NonSpineCatTargetError target) =
        printf "CAT target '%s' must have spine mode"
               (show (ppSugared target))
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
             | PolymorphicIdentityWarning
instance Show Warning where
    show StringDeprecatedWarning =
        "Type 'string' is deprecated. Use 'String' instead."
    show StringIdentityWarning =
        "Comparing String identity. Equality should be compared using 'equals'"
    show PolymorphicIdentityWarning =
        "Comparing polymorphic values is unstable. \n" ++
        "Later versions of Encore will require type constraints for this to work"

-- TODO: Refactor into hard errors (reuse TCError?)
newtype CCError = CCError (String, Backtrace)
instance Show CCError where
    show (CCError (msg, [])) =
        " *** Error during capturechecking *** \n" ++
        msg ++ "\n"
    show (CCError (msg, bt@((pos, _):_))) =
        " *** Error during capturechecking *** \n" ++
        show pos ++ "\n" ++
        msg ++ "\n" ++
        concatMap showBT bt
        where
          showBT (pos, node) =
              case show node of
                "" -> ""
                s  -> s ++ "\n"
