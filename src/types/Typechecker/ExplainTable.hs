{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Typechecker.ExplainTable (explain) where

import Typechecker.TypeError (Error(..), Warning(..))


class Explanainable a where
    explain :: a -> Maybe Int

instance Explanainable Error where
    explain err = toKey err

instance Explanainable Warning where
    explain warn = toKeyW warn





toKey :: Error -> Maybe Int
-- toKey (DistinctTypeParametersError _)                 = Just 1
-- toKey (WrongNumberOfMethodArgumentsError _ _ _ _)     = Just 2
-- toKey (WrongNumberOfFunctionArgumentsError _ _ _)     = Just 3
-- toKey (WrongNumberOfFunctionTypeArgumentsError _ _ _) = Just 4
-- toKey (WrongNumberOfTypeParametersError _ _ _ _)      = Just 5
-- toKey (MissingFieldRequirementError _ _)              = Just 6
-- toKey (CovarianceViolationError _ _ _)                = Just 7
-- toKey (RequiredFieldMismatchError _ _ _ _)            = Just 8
-- toKey (NonDisjointConjunctionError _ _ _)             = Just 9
-- toKey (OverriddenMethodTypeError _ _ _ _)             = Just 10
-- toKey (OverriddenMethodError _ _ _)                   = Just 11
-- toKey (IncludedMethodConflictError _ _ _)             = Just 12
-- toKey (MissingMethodRequirementError _ _)             = Just 13
toKey (MissingMainClass)                              = Just 14
-- toKey (SyncStreamCall)                                = Just 15
-- toKey (UnknownTraitError _)                           = Just 16
-- toKey (UnknownRefTypeError _)                         = Just 17
-- toKey (MalformedCapabilityError _)                    = Just 18
-- toKey (MalformedBoundError _)                         = Just 19
-- toKey (RecursiveTypesynonymError _)                   = Just 20
-- toKey (DuplicateThingError _ _)                       = Just 21
-- toKey (PassiveStreamingMethodError)                   = Just 22
-- toKey (PolymorphicConstructorError)                   = Just 23
-- toKey (StreamingConstructorError)                     = Just 24
-- toKey (MainMethodArgumentsError)                      = Just 25
-- toKey (MainConstructorError)                          = Just 26
-- toKey (FieldNotFoundError _ _)                        = Just 27
-- toKey (MethodNotFoundError _ _)                       = Just 28
-- toKey (BreakOutsideOfLoopError)                       = Just 29
-- toKey (BreakUsedAsExpressionError)                    = Just 30
-- toKey (ContinueOutsideOfLoopError)                    = Just 31
-- toKey (ContinueUsedAsExpressionError)                 = Just 32
-- toKey (NonCallableTargetError _)                      = Just 33
-- toKey (NonSendableTargetError _)                      = Just 34
-- toKey (MainMethodCallError)                           = Just 35
-- toKey (ConstructorCallError)                          = Just 36
-- toKey (ExpectingOtherTypeError _ _)                   = Just 37
-- toKey (NonStreamingContextError _)                    = Just 38
-- toKey (UnboundFunctionError _)                        = Just 39
-- toKey (NonFunctionTypeError _)                        = Just 40
-- toKey (BottomTypeInferenceError)                      = Just 41
-- toKey (IfInferenceError)                              = Just 42
-- toKey (IfBranchMismatchError _ _)                     = Just 43
-- toKey (EmptyMatchClauseError)                         = Just 44
-- toKey (ActiveMatchError)                              = Just 45
-- toKey (MatchInferenceError)                           = Just 46
-- toKey (ThisReassignmentError)                         = Just 47
-- toKey (ImmutableVariableError _)                      = Just 48
-- toKey (PatternArityMismatchError _ _ _)               = Just 49
-- toKey (PatternTypeMismatchError _ _)                  = Just 50
-- toKey (NonMaybeExtractorPatternError _)               = Just 51
-- toKey (InvalidPatternError _)                         = Just 52
-- toKey (InvalidTupleTargetError _ _ _)                 = Just 53
-- toKey (InvalidTupleAccessError _ _)                   = Just 54
-- toKey (CannotReadFieldError _)                        = Just 55
-- toKey (NonAssignableLHSError)                         = Just 56
-- toKey (ValFieldAssignmentError _ _)                   = Just 57
-- toKey (UnboundVariableError _)                        = Just 58
-- toKey (BuriedVariableError _)                         = Just 59
-- toKey (ObjectCreationError _)                         = Just 60
-- toKey (NonIterableError _)                            = Just 61
-- toKey (EmptyArrayLiteralError)                        = Just 62
-- toKey (NonIndexableError _)                           = Just 63
-- toKey (NonSizeableError _)                            = Just 64
-- toKey (FormatStringLiteralError)                      = Just 65
-- toKey (UnprintableExpressionError _)                  = Just 66
-- toKey (WrongNumberOfPrintArgumentsError _ _)          = Just 67
-- toKey (UnaryOperandMismatchError _ _)                 = Just 68
-- toKey (BinaryOperandMismatchError _ _ _ _)            = Just 69
-- toKey (UndefinedBinaryOperatorError _)                = Just 70
-- toKey (NullTypeInferenceError)                        = Just 71
-- toKey (CannotBeNullError _)                           = Just 72
toKey (TypeMismatchError _ _)                         = Just 73
-- toKey (TypeWithCapabilityMismatchError _ _ _)         = Just 74
-- toKey (TypeVariableAmbiguityError _ _ _)              = Just 75
-- toKey (FreeTypeVariableError _)                       = Just 76
-- toKey (TypeVariableAndVariableCommonNameError _)      = Just 77
-- toKey (UnionMethodAmbiguityError _ _)                 = Just 78
-- toKey (MalformedUnionTypeError _ _)                   = Just 79
-- toKey (RequiredFieldMutabilityError _ _)              = Just 80
-- toKey (ProvidingTraitFootprintError _ _ _ _)          = Just 81
-- toKey (TypeArgumentInferenceError _ _)                = Just 82
-- toKey (AmbiguousTypeError _ _)                        = Just 83
-- toKey (UnknownTypeUsageError _ _)                     = Just 84
-- toKey (AmbiguousNameError _ _)                        = Just 85
-- toKey (UnknownNamespaceError _)                       = Just 86
-- toKey (UnknownNameError _ _)                          = Just 87
-- toKey (ShadowedImportError _)                         = Just 88
-- toKey (WrongModuleNameError _ _)                      = Just 89
-- toKey (BadSyncCallError)                              = Just 90
-- toKey (PrivateAccessModifierTargetError _)            = Just 91
-- toKey (ClosureReturnError)                            = Just 92
-- toKey (ClosureForwardError)                           = Just 93
-- toKey (MatchMethodNonMaybeReturnError)                = Just 94
-- toKey (MatchMethodNonEmptyParameterListError)         = Just 95
-- toKey (ImpureMatchMethodError _)                      = Just 96
-- toKey (IdComparisonNotSupportedError _)               = Just 97
-- toKey (IdComparisonTypeMismatchError _ _)             = Just 98
-- toKey (ForwardInPassiveContext _)                     = Just 99
-- toKey (ForwardInFunction)                             = Just 100
-- toKey (ForwardTypeError _ _)                          = Just 101
-- toKey (ForwardTypeClosError _ _)                      = Just 102
-- toKey (CannotHaveModeError _)                         = Just 103
-- toKey (ModelessError _)                               = Just 104
-- toKey (ModeOverrideError _)                           = Just 105
-- toKey (CannotConsumeError _)                          = Just 106
-- toKey (CannotConsumeTypeError _)                      = Just 107
-- toKey (ImmutableConsumeError _)                       = Just 108
-- toKey (CannotGiveReadModeError _)                     = Just 109
-- toKey (CannotGiveSharableModeError _)                 = Just 110
-- toKey (NonValInReadContextError _)                    = Just 111
-- toKey (NonSafeInReadContextError _ _)                 = Just 112
-- toKey (NonSafeInExtendedReadTraitError _ _ _)         = Just 113
-- toKey (ProvidingToReadTraitError _ _ _)               = Just 114
-- toKey (SubordinateReturnError _ _)                    = Just 115
-- toKey (SubordinateArgumentError _)                    = Just 116
-- toKey (SubordinateFieldError _)                       = Just 117
-- toKey (ThreadLocalFieldError _)                       = Just 118
-- toKey (ThreadLocalFieldExtensionError _ _)            = Just 119
-- toKey (ThreadLocalArgumentError _)                    = Just 120
-- toKey (PolymorphicArgumentSendError _ _)              = Just 121
-- toKey (PolymorphicReturnError _ _)                    = Just 122
-- toKey (ThreadLocalReturnError _ _)                    = Just 123
-- toKey (MalformedConjunctionError _ _ _)               = Just 124
-- toKey (CannotUnpackError _)                           = Just 125
-- toKey (CannotInferUnpackingError _)                   = Just 126
-- toKey (UnsplittableTypeError _)                       = Just 127
-- toKey (DuplicatingSplitError _)                       = Just 128
-- toKey (StackboundArrayTypeError _)                    = Just 129
-- toKey (ManifestConflictError _ _)                     = Just 130
-- toKey (ManifestClassConflictError _ _)                = Just 131
-- toKey (UnmodedMethodExtensionError _ _)               = Just 132
-- toKey (ActiveTraitError _ _)                          = Just 133
-- toKey (NewWithModeError)                              = Just 134
-- toKey (UnsafeTypeArgumentError _ _)                   = Just 135
-- toKey (OverlapWithBuiltins)                           = Just 136
-- toKey (SimpleError _)                                 = Just 137
-- toKey (ReverseBorrowingError)                         = Just 138
-- toKey (BorrowedFieldError _)                          = Just 139
-- toKey (LinearClosureError _ _)                        = Just 140
-- toKey (BorrowedLeakError _)                           = Just 141
-- toKey (NonBorrowableError _)                          = Just 142
-- toKey (ActiveBorrowError _ _)                         = Just 143
-- toKey (ActiveBorrowSendError _ _)                     = Just 144
-- toKey (DuplicateBorrowError _)                        = Just 145
-- toKey (StackboundednessMismatchError _ _)             = Just 146
-- toKey (LinearCaptureError _ _)                        = Just 147
toKey _                                               = Nothing

toKeyW :: Warning -> Maybe Int
-- toKeyW (StringDeprecatedWarning)                      = Just 1
-- toKeyW (StringIdentityWarning)                        = Just 2
-- toKeyW (PolymorphicIdentityWarning)                   = Just 3
-- toKeyW (ShadowedMethodWarning _)                      = Just 4
-- toKeyW (ExpressionResultIgnoredWarning _)             = Just 5
-- toKeyW (ArrayTypeArgumentWarning)                     = Just 6
-- toKeyW (ArrayInReadContextWarning)                    = Just 7
-- toKeyW (SharedArrayWarning)                           = Just 8
-- toKeyW (CapabilitySplitWarning)                       = Just 9
toKeyW _                                              = Nothing