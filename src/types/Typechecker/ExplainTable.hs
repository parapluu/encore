{-# LANGUAGE OverloadedStrings #-}

module Typechecker.ExplainTable (
     Table
    ,lookupHash
    ,getErrorExplanation
    ) where

import Typechecker.TypeError

import Text.PrettyPrint.Annotated
import Text.Read (readMaybe)
import Text.Printf (printf)


lookupHash :: Error -> Maybe Int
lookupHash err
    | Just k <- toKey err = let T t = table in lookup' t k 1
    | otherwise = Nothing
    where
        lookup' [] k _ = Nothing
        lookup' ((k', v):as) k x
            |  k == k'= Just x
            | otherwise = lookup' as k (x+1)


getErrorExplanation :: String -> Maybe (Doc a)
getErrorExplanation ('E':err) =
    case readMaybe err :: Maybe Int of
        Just num
            | num > 0 -> let T t = table in lookupExplain (num-1) t
            | otherwise -> Nothing
        Nothing -> Nothing
getErrorExplanation _ = Nothing


lookupExplain _ [] = Nothing
lookupExplain 0 ((_, v):_) = Just v
lookupExplain x (_:ls) =  lookupExplain (x-1) ls 


newtype Table k v = T [(k, v)]

table :: Table String (Doc a)
table = 
    T [
        (
            "MissingMainClass",
            "Welcome to the Encore Compiler!" $$
            "Here you will meet many wonderful methods and functions and whatnot!"
        )
    ]


-- I want to have a guarding Nothing in case if an error is introdused,
-- but the compiler gives me an overlapped-error....
toKey :: Error -> Maybe String
toKey (DistinctTypeParametersError _)                 = Just "DistinctTypeParametersError"
toKey (WrongNumberOfMethodArgumentsError _ _ _ _)     = Just "WrongNumberOfMethodArgumentsError"
toKey (WrongNumberOfFunctionArgumentsError _ _ _)     = Just "WrongNumberOfFunctionArgumentsError"
toKey (WrongNumberOfFunctionTypeArgumentsError _ _ _) = Just "WrongNumberOfFunctionTypeArgumentsError"
toKey (WrongNumberOfTypeParametersError _ _ _ _)      = Just "WrongNumberOfTypeParametersError"
toKey (MissingFieldRequirementError _ _)              = Just "MissingFieldRequirementError"
toKey (CovarianceViolationError _ _ _)                = Just "CovarianceViolationError"
toKey (RequiredFieldMismatchError _ _ _ _)            = Just "RequiredFieldMismatchError"
toKey (NonDisjointConjunctionError _ _ _)             = Just "NonDisjointConjunctionError"
toKey (OverriddenMethodTypeError _ _ _ _)             = Just "OverriddenMethodTypeError"
toKey (OverriddenMethodError _ _ _)                   = Just "OverriddenMethodError"
toKey (IncludedMethodConflictError _ _ _)             = Just "IncludedMethodConflictError"
toKey (MissingMethodRequirementError _ _)             = Just "MissingMethodRequirementError"
toKey (MissingMainClass)                              = Just "MissingMainClass"
toKey (SyncStreamCall)                                = Just "SyncStreamCall"
toKey (UnknownTraitError _)                           = Just "UnknownTraitError"
toKey (UnknownRefTypeError _)                         = Just "UnknownRefTypeError"
toKey (MalformedCapabilityError _)                    = Just "MalformedCapabilityError"
toKey (MalformedBoundError _)                         = Just "MalformedBoundError"
toKey (RecursiveTypesynonymError _)                   = Just "RecursiveTypesynonymError"
toKey (DuplicateThingError _ _)                       = Just "DuplicateThingError"
toKey (PassiveStreamingMethodError)                   = Just "PassiveStreamingMethodError"
toKey (PolymorphicConstructorError)                   = Just "PolymorphicConstructorError"
toKey (StreamingConstructorError)                     = Just "StreamingConstructorError"
toKey (MainMethodArgumentsError)                      = Just "MainMethodArgumentsError"
toKey (MainConstructorError)                          = Just "MainConstructorError"
toKey (FieldNotFoundError _ _)                        = Just "FieldNotFoundError"
toKey (MethodNotFoundError _ _)                       = Just "MethodNotFoundError"
toKey (BreakOutsideOfLoopError)                       = Just "BreakOutsideOfLoopError"
toKey (BreakUsedAsExpressionError)                    = Just "BreakUsedAsExpressionError"
toKey (ContinueOutsideOfLoopError)                    = Just "ContinueOutsideOfLoopError"
toKey (ContinueUsedAsExpressionError)                 = Just "ContinueUsedAsExpressionError"
toKey (NonCallableTargetError _)                      = Just "NonCallableTargetError"
toKey (NonSendableTargetError _)                      = Just "NonSendableTargetError"
toKey (MainMethodCallError)                           = Just "MainMethodCallError"
toKey (ConstructorCallError)                          = Just "ConstructorCallError"
toKey (ExpectingOtherTypeError _ _)                   = Just "ExpectingOtherTypeError"
toKey (NonStreamingContextError _)                    = Just "NonStreamingContextError"
toKey (UnboundFunctionError _)                        = Just "UnboundFunctionError"
toKey (NonFunctionTypeError _)                        = Just "NonFunctionTypeError"
toKey (BottomTypeInferenceError)                      = Just "BottomTypeInferenceError"
toKey (IfInferenceError)                              = Just "IfInferenceError"
toKey (IfBranchMismatchError _ _)                     = Just "IfBranchMismatchError"
toKey (EmptyMatchClauseError)                         = Just "EmptyMatchClauseError"
toKey (ActiveMatchError)                              = Just "ActiveMatchError"
toKey (MatchInferenceError)                           = Just "MatchInferenceError"
toKey (ThisReassignmentError)                         = Just "ThisReassignmentError"
toKey (ImmutableVariableError _)                      = Just "ImmutableVariableError"
toKey (PatternArityMismatchError _ _ _)               = Just "PatternArityMismatchError"
toKey (PatternTypeMismatchError _ _)                  = Just "PatternTypeMismatchError"
toKey (NonMaybeExtractorPatternError _)               = Just "NonMaybeExtractorPatternError"
toKey (InvalidPatternError _)                         = Just "InvalidPatternError"
toKey (InvalidTupleTargetError _ _ _)                 = Just "InvalidTupleTargetError"
toKey (InvalidTupleAccessError _ _)                   = Just "InvalidTupleAccessError"
toKey (CannotReadFieldError _)                        = Just "CannotReadFieldError"
toKey (NonAssignableLHSError)                         = Just "NonAssignableLHSError"
toKey (ValFieldAssignmentError _ _)                   = Just "ValFieldAssignmentError"
toKey (UnboundVariableError _)                        = Just "UnboundVariableError"
toKey (BuriedVariableError _)                         = Just "BuriedVariableError"
toKey (ObjectCreationError _)                         = Just "ObjectCreationError"
toKey (NonIterableError _)                            = Just "NonIterableError"
toKey (EmptyArrayLiteralError)                        = Just "EmptyArrayLiteralError"
toKey (NonIndexableError _)                           = Just "NonIndexableError"
toKey (NonSizeableError _)                            = Just "NonSizeableError"
toKey (FormatStringLiteralError)                      = Just "FormatStringLiteralError"
toKey (UnprintableExpressionError _)                  = Just "UnprintableExpressionError"
toKey (WrongNumberOfPrintArgumentsError _ _)          = Just "WrongNumberOfPrintArgumentsError"
toKey (UnaryOperandMismatchError _ _)                 = Just "UnaryOperandMismatchError"
toKey (BinaryOperandMismatchError _ _ _ _)            = Just "BinaryOperandMismatchError"
toKey (UndefinedBinaryOperatorError _)                = Just "UndefinedBinaryOperatorError"
toKey (NullTypeInferenceError)                        = Just "NullTypeInferenceError"
toKey (CannotBeNullError _)                           = Just "CannotBeNullError"
toKey (TypeMismatchError _ _)                         = Just "TypeMismatchError"
toKey (TypeWithCapabilityMismatchError _ _ _)         = Just "TypeWithCapabilityMismatchError"
toKey (TypeVariableAmbiguityError _ _ _)              = Just "TypeVariableAmbiguityError"
toKey (FreeTypeVariableError _)                       = Just "FreeTypeVariableError"
toKey (TypeVariableAndVariableCommonNameError _)      = Just "TypeVariableAndVariableCommonNameError"
toKey (UnionMethodAmbiguityError _ _)                 = Just "UnionMethodAmbiguityError"
toKey (MalformedUnionTypeError _ _)                   = Just "MalformedUnionTypeError"
toKey (RequiredFieldMutabilityError _ _)              = Just "RequiredFieldMutabilityError"
toKey (ProvidingTraitFootprintError _ _ _ _)          = Just "ProvidingTraitFootprintError"
toKey (TypeArgumentInferenceError _ _)                = Just "TypeArgumentInferenceError"
toKey (AmbiguousTypeError _ _)                        = Just "AmbiguousTypeError"
toKey (UnknownTypeUsageError _ _)                     = Just "UnknownTypeUsageError"
toKey (AmbiguousNameError _ _)                        = Just "AmbiguousNameError"
toKey (UnknownNamespaceError _)                       = Just "UnknownNamespaceError"
toKey (UnknownNameError _ _)                          = Just "UnknownNameError"
toKey (ShadowedImportError _)                         = Just "ShadowedImportError"
toKey (WrongModuleNameError _ _)                      = Just "WrongModuleNameError"
toKey (BadSyncCallError)                              = Just "BadSyncCallError"
toKey (PrivateAccessModifierTargetError _)            = Just "PrivateAccessModifierTargetError"
toKey (ClosureReturnError)                            = Just "ClosureReturnError"
toKey (ClosureForwardError)                           = Just "ClosureForwardError"
toKey (MatchMethodNonMaybeReturnError)                = Just "MatchMethodNonMaybeReturnError"
toKey (MatchMethodNonEmptyParameterListError)         = Just "MatchMethodNonEmptyParameterListError"
toKey (ImpureMatchMethodError _)                      = Just "ImpureMatchMethodError"
toKey (IdComparisonNotSupportedError _)               = Just "IdComparisonNotSupportedError"
toKey (IdComparisonTypeMismatchError _ _)             = Just "IdComparisonTypeMismatchError"
toKey (ForwardInPassiveContext _)                     = Just "ForwardInPassiveContext"
toKey (ForwardInFunction)                             = Just "ForwardInFunction"
toKey (ForwardTypeError _ _)                          = Just "ForwardTypeError"
toKey (ForwardTypeClosError _ _)                      = Just "ForwardTypeClosError"
toKey (CannotHaveModeError _)                         = Just "CannotHaveModeError"
toKey (ModelessError _)                               = Just "ModelessError"
toKey (ModeOverrideError _)                           = Just "ModeOverrideError"
toKey (CannotConsumeError _)                          = Just "CannotConsumeError"
toKey (CannotConsumeTypeError _)                      = Just "CannotConsumeTypeError"
toKey (ImmutableConsumeError _)                       = Just "ImmutableConsumeError"
toKey (CannotGiveReadModeError _)                     = Just "CannotGiveReadModeError"
toKey (CannotGiveSharableModeError _)                 = Just "CannotGiveSharableModeError"
toKey (NonValInReadContextError _)                    = Just "NonValInReadContextError"
toKey (NonSafeInReadContextError _ _)                 = Just "NonSafeInReadContextError"
toKey (NonSafeInExtendedReadTraitError _ _ _)         = Just "NonSafeInExtendedReadTraitError"
toKey (ProvidingToReadTraitError _ _ _)               = Just "ProvidingToReadTraitError"
toKey (SubordinateReturnError _ _)                    = Just "SubordinateReturnError"
toKey (SubordinateArgumentError _)                    = Just "SubordinateArgumentError"
toKey (SubordinateFieldError _)                       = Just "SubordinateFieldError"
toKey (ThreadLocalFieldError _)                       = Just "ThreadLocalFieldError"
toKey (ThreadLocalFieldExtensionError _ _)            = Just "ThreadLocalFieldExtensionError"
toKey (ThreadLocalArgumentError _)                    = Just "ThreadLocalArgumentError"
toKey (PolymorphicArgumentSendError _ _)              = Just "PolymorphicArgumentSendError"
toKey (PolymorphicReturnError _ _)                    = Just "PolymorphicReturnError"
toKey (ThreadLocalReturnError _ _)                    = Just "ThreadLocalReturnError"
toKey (MalformedConjunctionError _ _ _)               = Just "MalformedConjunctionError"
toKey (CannotUnpackError _)                           = Just "CannotUnpackError"
toKey (CannotInferUnpackingError _)                   = Just "CannotInferUnpackingError"
toKey (UnsplittableTypeError _)                       = Just "UnsplittableTypeError"
toKey (DuplicatingSplitError _)                       = Just "DuplicatingSplitError"
toKey (StackboundArrayTypeError _)                    = Just "StackboundArrayTypeError"
toKey (ManifestConflictError _ _)                     = Just "ManifestConflictError"
toKey (ManifestClassConflictError _ _)                = Just "ManifestClassConflictError"
toKey (UnmodedMethodExtensionError _ _)               = Just "UnmodedMethodExtensionError"
toKey (ActiveTraitError _ _)                          = Just "ActiveTraitError"
toKey (NewWithModeError)                              = Just "NewWithModeError"
toKey (UnsafeTypeArgumentError _ _)                   = Just "UnsafeTypeArgumentError"
toKey (OverlapWithBuiltins)                           = Just "OverlapWithBuiltins"
toKey (SimpleError _)                                 = Just "SimpleError"
toKey (ReverseBorrowingError)                         = Just "ReverseBorrowingError"
toKey (BorrowedFieldError _)                          = Just "BorrowedFieldError"
toKey (LinearClosureError _ _)                        = Just "LinearClosureError"
toKey (BorrowedLeakError _)                           = Just "BorrowedLeakError"
toKey (NonBorrowableError _)                          = Just "NonBorrowableError"
toKey (ActiveBorrowError _ _)                         = Just "ActiveBorrowError"
toKey (ActiveBorrowSendError _ _)                     = Just "ActiveBorrowSendError"
toKey (DuplicateBorrowError _)                        = Just "DuplicateBorrowError"
toKey (StackboundednessMismatchError _ _)             = Just "StackboundednessMismatchError"
toKey (LinearCaptureError _ _)                        = Just "LinearCaptureError"
--toKey _                                               = Nothing