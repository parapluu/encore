{-# LANGUAGE ConstrainedClassMethods #-}

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
                             ,currentMethodFromBacktrace
                             ,currentFunctionFromBacktrace) where

import Text.PrettyPrint
import Text.Megaparsec(SourcePos)
import Data.Maybe
import Data.List
import Text.Printf (printf)

import Identifiers
import Types
import AST.AST hiding (showWithKind)
import AST.PrettyPrinter
import AST.Meta(showSourcePos)

data BacktraceNode = BTFunction Name Type
                   | BTTrait Type
                   | BTClass Type
                   | BTParam ParamDecl
                   | BTField FieldDecl
                   | BTMethod MethodDecl
                   | BTExpr Expr
                   | BTTypedef Type
                   | BTModule Name
                   | BTImport Namespace


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
  show (BTExpr expr)
    | (isNothing . getSugared) expr = ""
    | otherwise =
      let str = show $ nest 2 $ ppSugared expr
      in "In expression: \n" ++ str
  show (BTTypedef tl) =
     concat ["In typedef '", show tl, "'"]
  show (BTModule m) =
     concat ["In declaration of module '", show m, "'"]
  show (BTImport ns) =
     concat ["In import of module '", show ns, "'"]

type Backtrace = [(SourcePos, BacktraceNode)]
emptyBT :: Backtrace
emptyBT = []

currentMethodFromBacktrace :: Backtrace -> Maybe MethodDecl
currentMethodFromBacktrace [] = Nothing
currentMethodFromBacktrace ((_, BTExpr Closure{}):_) = Nothing
currentMethodFromBacktrace ((_, BTExpr Async{}):_) = Nothing
currentMethodFromBacktrace ((_, BTMethod m):_) = Just m
currentMethodFromBacktrace (_:bt) = currentMethodFromBacktrace bt

currentFunctionFromBacktrace :: Backtrace -> Maybe (Name, Type)
currentFunctionFromBacktrace [] = Nothing
currentFunctionFromBacktrace ((_, BTExpr Closure{}):_) = Nothing
currentFunctionFromBacktrace ((_, BTExpr Async{}):_) = Nothing
currentFunctionFromBacktrace ((_, BTFunction n t):_) = Just (n, t)
currentFunctionFromBacktrace (_:bt) = currentFunctionFromBacktrace bt

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

instance Pushable Expr where
    push expr = pushMeta expr (BTExpr expr)

instance Pushable Typedef where
    push t@(Typedef {typedefdef}) = pushMeta t (BTTypedef typedefdef)

instance Pushable ModuleDecl where
    push m@(Module{modname}) = pushMeta m (BTModule modname)

instance Pushable ImportDecl where
    push i@(Import{itarget}) = pushMeta i (BTImport itarget)

refTypeName :: Type -> String
refTypeName ty
    | isClassType ty = "class '" ++ getId ty ++ "'"
    | isTraitType ty = "trait '" ++ getId ty ++ "'"
    | isCapabilityType ty = "capability '" ++ show ty ++ "'"
    | isUnionType ty = "union '" ++ show ty ++ "'"
    | otherwise = error $ "Util.hs: No refTypeName for " ++
                          showWithKind ty

-- | The data type for a type checking error. Showing it will
-- produce an error message and print the backtrace.
data TCError = TCError Error Backtrace
instance Show TCError where
    show (TCError err []) =
        " *** Error during typechecking *** \n" ++
        show err ++ "\n"
    show (TCError err bt@((pos, _):_)) =
        " *** Error during typechecking *** \n" ++
        showSourcePos pos ++ "\n" ++
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
  | WrongNumberOfFunctionArgumentsError QualifiedName Int Int
  | WrongNumberOfFunctionTypeArgumentsError QualifiedName Int Int
  | WrongNumberOfTypeParametersError Type Int Type Int
  | MissingFieldRequirementError FieldDecl Type
  | CovarianceViolationError FieldDecl Type Type
  | RequiredFieldMismatchError FieldDecl Type Type Bool
  | NonDisjointConjunctionError Type Type FieldDecl
  | OverriddenMethodTypeError Name Type Type
  | OverriddenMethodError Name Type Error
  | IncludedMethodConflictError Name Type Type
  | MissingMethodRequirementError FunctionHeader Type
  | MissingMainClass
  | SyncStreamCall
  | UnknownTraitError Type
  | UnknownRefTypeError Type
  | MalformedCapabilityError Type
  | RecursiveTypesynonymError Type
  | DuplicateThingError String String
  | PassiveStreamingMethodError
  | PolymorphicConstructorError
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
  | UnboundFunctionError QualifiedName
  | NonFunctionTypeError Type
  | BottomTypeInferenceError
  | IfInferenceError
  | IfBranchMismatchError Type Type
  | EmptyMatchClauseError
  | ActiveMatchError
  | MatchInferenceError
  | ThisReassignmentError
  | ImmutableVariableError QualifiedName
  | PatternArityMismatchError Name Int Int
  | PatternTypeMismatchError Expr Type
  | NonMaybeExtractorPatternError Expr
  | InvalidPatternError Expr
  | InvalidTupleTargetError Expr Int Type
  | InvalidTupleAccessError Expr Int
  | CannotReadFieldError Expr
  | NonAssignableLHSError
  | ValFieldAssignmentError Name Type
  | UnboundVariableError QualifiedName
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
  | TypeVariableAndVariableCommonNameError [Name]
  | UnionMethodAmbiguityError Type Name
  | MalformedUnionTypeError Type Type
  | ConcreteTypeParameterError Type
  | ProvidingTraitFootprintError Type Type Name [FieldDecl]
  | TypeArgumentInferenceError Expr Type
  | AmbiguousTypeError Type [Type]
  | UnknownTypeUsageError String Type
  | AmbiguousNameError QualifiedName [(QualifiedName, Type)]
  | UnknownNamespaceError (Maybe Namespace)
  | UnknownNameError Namespace Name
  | ShadowedImportError ImportDecl
  | WrongModuleNameError Name FilePath
  | PrivateAccessModifierTargetError Name
  | ClosureReturnError
  | MatchMethodNonMaybeReturnError
  | MatchMethodNonEmptyParameterListError
  | ImpureMatchMethodError Expr
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
              (if name == constructorName
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
    show (OverriddenMethodTypeError name expected trait) =
        printf ("Overridden method '%s' does not " ++
                "have the expected type '%s' required by %s")
               (show name) (show expected) (refTypeName trait)
    show (OverriddenMethodError name trait err) =
        case err of
          FieldNotFoundError f _ ->
            printf ("Overridden method '%s' requires access to field '%s' " ++
                    "which is not in requiring %s.\n" ++
                    "Consider extending the trait on inclusion: %s(%s)")
                   (show name) (show f) (refTypeName trait) (show trait) (show f)
          MethodNotFoundError m _ ->
            printf ("Overridden method '%s' calls method '%s' " ++
                    "which is not in requiring %s.\n" ++
                    "Consider extending the trait on inclusion: %s(%s())")
                   (show name) (show m) (refTypeName trait) (show trait) (show m)
          TypeMismatchError actual expected ->
            if actual == abstractTraitFromTraitType trait
            then printf ("Overridden method '%s' uses 'this' as %s " ++
                         "and cannot be typechecked in requiring %s")
                        (show name) (show expected) (refTypeName trait)
            else defaultMessage
          ValFieldAssignmentError f targetType ->
            if targetType == abstractTraitFromTraitType trait
            then printf ("Overridden method '%s' writes field '%s' " ++
                         "which is marked as immutable in requiring %s.")
                         (show name) (show f) (refTypeName trait)
            else defaultMessage
          err -> defaultMessage
        where
          defaultMessage =
            printf ("Overridden method '%s' cannot be typechecked in " ++
                    "requiring %s:\n%s")
                   (show name) (refTypeName trait) (show err)

    show (IncludedMethodConflictError name left right) =
        printf "Conflicting inclusion of method '%s' from %s and %s"
               (show name) (refTypeName left) (refTypeName right)
    show (MissingMethodRequirementError header trait) =
        printf "Cannot find method '%s' required by included %s"
               (show $ ppFunctionHeader header) (refTypeName trait)
    show (UnknownTraitError ty) =
        printf "Couldn't find trait '%s'" (getId ty)
    show MissingMainClass = "Couldn't find active class 'Main'"
    show SyncStreamCall = "A stream method can not be called synchronously since it will invariably deadlock"
    show (PrivateAccessModifierTargetError name) =
        printf "Cannot call private %s" kind
     where
       kind = if name == constructorName
              then "constructor"
              else "method '" ++ show name ++ "'"
    show (UnknownRefTypeError ty) =
        printf "Couldn't find class, trait or typedef '%s'" (show ty)
    show (MalformedCapabilityError ty) =
        printf "Cannot form capability with %s" (showWithKind ty)
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
        let nameWithKind = if name == constructorName
                           then "constructor"
                           else "method '" ++ show name ++ "'"
            targetType = if isRefType ty
                         then refTypeName ty
                         else showWithKind ty
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
    show (ImmutableVariableError qname) =
        printf "Variable '%s' is immutable and cannot be re-assigned"
               (show qname)
    show (PatternArityMismatchError name expected actual) =
        printf "Extractor '%s' returns %s. Pattern has %s"
               (show name)
               (if expected == 1
                then "1 value"
                else show expected ++ " values")
               (show actual)
    show (PatternTypeMismatchError pattern ty) =
        printf "Pattern '%s' does not match expected type '%s'"
               (show $ ppSugared pattern) (show ty)
    show (NonMaybeExtractorPatternError pattern) =
        printf "Extractor '%s' must return a Maybe type to be used as a pattern"
               (show $ ppSugared pattern)
    show (InvalidPatternError pattern) =
        printf "'%s' is not a valid pattern"
               (show $ ppSugared pattern)
    show (InvalidTupleTargetError target compartment ty) =
        printf "Compartment access %s.%d expects a tuple target, found %s"
               (show $ ppSugared target)
               compartment
               (show $ ty)
    show (InvalidTupleAccessError target compartment) =
        printf "No .%d compartment in tuple %s"
               compartment
               (show $ ppSugared target)
    show (CannotReadFieldError target) =
        printf "Cannot read field of expression '%s' of %s"
          (show $ ppSugared target) (showWithKind $ getType target)
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
    show (TypeVariableAndVariableCommonNameError [name]) =
        printf "Type variable '%s' clashes with existing variable name."
               (show name)
    show (TypeVariableAndVariableCommonNameError names) =
        printf "Type variables %s clash with existing variable names."
               formattingName
        where
          formattingName =
            let ns = map (\n -> "'" ++ show n ++ "', ") (init names)
                lastName = "'" ++ (show $ last names) ++ "'"
            in (show ns) ++ "and " ++ lastName
    show (UnionMethodAmbiguityError ty name) =
        printf "Cannot disambiguate method '%s' in %s"
               (show name) (showWithKind ty)
    show (MalformedUnionTypeError ty union) =
        printf "Type '%s' is not compatible with %s"
               (show ty) (showWithKind union)
    show (ConcreteTypeParameterError ty) =
        printf "Concrete type '%s' cannot be used as a type parameter"
               (show ty)
    show (TypeArgumentInferenceError call param) =
        printf "Cannot infer the type of parameter '%s' of %s '%s'"
               (show param) kind calledName
        where
          mname = name call
          kind | isFunctionCall call = "function"
               | isMethodCall call =
                   if mname == constructorName
                   then "class"
                   else "method"
               | otherwise = error msg
          calledName | isFunctionCall call = show $ qname call
                     | isMethodCall call =
                         if mname == constructorName
                         then show $ getType (target call)
                         else show mname
                     | otherwise = error msg
          msg = "TypeError.hs: " ++ show call ++
                " is not a function or method call"
    show (ProvidingTraitFootprintError provider requirer mname fields) =
        printf ("Trait '%s' cannot provide method '%s' to trait '%s'.\n" ++
                "'%s' can mutate fields that are marked immutable in '%s':\n%s")
               (show provider) (show mname) (show requirer)
               (show provider) (show requirer)
               (unlines (map (("  " ++) . show) fields))
    show (AmbiguousTypeError ty candidates) =
        printf "Ambiguous reference to %s. Possible candidates are:\n%s"
               (showWithKind ty) (unlines $ map (("  " ++) . show) candidates)
    show (UnknownTypeUsageError usage ty) =
        printf "Cannot %s unimported type %s"
               usage (show ty)
    show (AmbiguousNameError qname candidates) =
        printf "Ambiguous reference to function %s. Possible candidates are:\n%s"
               (show qname) candidateList
        where
          candidateList =
            unlines $ map (("  " ++) . showCandidate) candidates
          showCandidate (qn, ty) = show qn ++ " : " ++ show ty
    show (UnknownNamespaceError maybeNs) =
        printf "Unknown namespace %s"
               (maybe "" show maybeNs)
    show (UnknownNameError ns name) =
        printf "Module %s has no function or type called '%s'"
               (show ns) (show name)
    show (ShadowedImportError i) =
        printf "Introduction of module alias '%s' shadows existing import"
               (show $ itarget i)
    show (WrongModuleNameError modname expected) =
        printf "Module name '%s' and file name '%s' must match"
               (show modname) expected
    show PolymorphicConstructorError =
        printf "Constructors (a.k.a. 'init methods') cannot use parametric methods"
    show ClosureReturnError =
        printf "Cannot use 'return' in a closure"
    show MatchMethodNonMaybeReturnError =
        "Match methods must return a Maybe type"
    show MatchMethodNonEmptyParameterListError =
        "Match methods cannot have parameters"
    show (ImpureMatchMethodError e) =
        printf "Match methods must be pure%s"
               pointer
        where
          pointer
            | While{} <- e = ". Consider using a for loop"
            | otherwise = ""
    show (SimpleError msg) = msg


data TCWarning = TCWarning Backtrace Warning
instance Show TCWarning where
    show (TCWarning [] w) =
        "Warning:\n" ++
        show w
    show (TCWarning ((pos, _):_) w) =
        "Warning at " ++ showSourcePos pos ++ ":\n" ++
        show w

data Warning = StringDeprecatedWarning
             | StringIdentityWarning
             | PolymorphicIdentityWarning
             | ShadowedMethodWarning FieldDecl
instance Show Warning where
    show StringDeprecatedWarning =
        "Type 'string' is deprecated. Use 'String' instead."
    show StringIdentityWarning =
        "Comparing String identity. Equality should be compared using 'equals'"
    show PolymorphicIdentityWarning =
        "Comparing polymorphic values is unstable. \n" ++
        "Later versions of Encore will require type constraints for this to work"
    show (ShadowedMethodWarning Field{fname, ftype}) =
        printf ("Field '%s' holds %s and could be confused with " ++
                "the method of the same name")
               (show fname) (if isArrayType ftype
                             then "an array"
                             else "a function")