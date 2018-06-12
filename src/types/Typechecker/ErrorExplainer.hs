
module Typechecker.ErrorExplainer where

--import Typechecker.TypeError(TCError(TCError),TCWarning(TCWarning))
import Text.PrettyPrint hiding(brackets)
import qualified Data.Map.Strict as Map
import Data.Char (digitToInt)
import Text.Read (readMaybe)

--hash (UnionMethodAmbiguityError _ _) = 3

--explain 3 = "stuff"



getErrorindex :: String -> Maybe Int
getErrorindex k = case Map.lookupIndex k createMap of
    Nothing -> Nothing
    Just x -> Just (x+1)


getErrorExplanation :: String -> Maybe String
getErrorExplanation ('E':err) = let map = createMap in
    case readMaybe err :: Maybe Int of
        Just num
            | num > 0 && num <= Map.size map -> Just $ snd $ Map.elemAt (num-1) map
            | otherwise -> Nothing
        Nothing -> Nothing
getErrorExplanation _ = Nothing



{-
    This code is probably highly inefficient. Should be refactored in the future.
    It should not be neccesary to recreate this list every time during runtime, 
    either use template haskell or place it somewhere useful.
-}
createMap :: Map.Map String String
createMap = Map.fromList [
    ("NonAssignableLHSError","This is a test...")
    , ("BinaryOperandMismatchError","This is an error for BinaryOperandMismatchError, but its only a test, nothing more :)")
    ]

-- toHashNum :: Error -> Maybe Int
-- toHashNum (DistinctTypeParametersError _)                 = Just 1
-- toHashNum (WrongNumberOfMethodArgumentsError _ _ _ _)     = Just 2
-- toHashNum (WrongNumberOfFunctionArgumentsError _ _ _)     = Just 3
-- toHashNum (WrongNumberOfFunctionTypeArgumentsError _ _ _) = Just 4
-- toHashNum (WrongNumberOfTypeParametersError _ _ _ _)      = Just 5
-- toHashNum (MissingFieldRequirementError _ _)              = Just 6
-- toHashNum (CovarianceViolationError _ _ _)                = Just 7
-- toHashNum (RequiredFieldMismatchError _ _ _ _)            = Just 8
-- toHashNum (NonDisjointConjunctionError _ _ _)             = Just 9
-- toHashNum (OverriddenMethodTypeError _ _ _ _)             = Just 10
-- toHashNum (OverriddenMethodError _ _ _)                   = Just 11
-- toHashNum (IncludedMethodConflictError _ _ _)             = Just 12
-- toHashNum (MissingMethodRequirementError _ _)             = Just 13
-- toHashNum (MissingMainClass)                              = Just 14
-- toHashNum (SyncStreamCall)                                = Just 15
-- toHashNum (UnknownTraitError _)                           = Just 16
-- toHashNum (UnknownRefTypeError _)                         = Just 17
-- toHashNum (MalformedCapabilityError _)                    = Just 18
-- toHashNum (MalformedBoundError _)                         = Just 19
-- toHashNum (RecursiveTypesynonymError _)                   = Just 20
-- toHashNum (DuplicateThingError _ _)                       = Just 21
-- toHashNum (PassiveStreamingMethodError)                   = Just 22
-- toHashNum (PolymorphicConstructorError)                   = Just 23
-- toHashNum (StreamingConstructorError)                     = Just 24
-- toHashNum (MainMethodArgumentsError)                      = Just 25
-- toHashNum (MainConstructorError)                          = Just 26
-- toHashNum (FieldNotFoundError _ _)                        = Just 27
-- toHashNum (MethodNotFoundError _ _)                       = Just 28
-- toHashNum (BreakOutsideOfLoopError)                       = Just 29
-- toHashNum (BreakUsedAsExpressionError)                    = Just 30
-- toHashNum (ContinueOutsideOfLoopError)                    = Just 31
-- toHashNum (ContinueUsedAsExpressionError)                 = Just 32
-- toHashNum (NonCallableTargetError _)                      = Just 33
-- toHashNum (NonSendableTargetError _)                      = Just 34
-- toHashNum (MainMethodCallError)                           = Just 35
-- toHashNum (ConstructorCallError)                          = Just 36
-- toHashNum (ExpectingOtherTypeError _ _)                   = Just 37
-- toHashNum (NonStreamingContextError _)                    = Just 38
-- toHashNum (UnboundFunctionError _)                        = Just 39
-- toHashNum (NonFunctionTypeError _)                        = Just 40
-- toHashNum (BottomTypeInferenceError)                      = Just 41
-- toHashNum (IfInferenceError)                              = Just 42
-- toHashNum (IfBranchMismatchError _ _)                     = Just 43
-- toHashNum (EmptyMatchClauseError)                         = Just 44
-- toHashNum (ActiveMatchError)                              = Just 45
-- toHashNum (MatchInferenceError)                           = Just 46
-- toHashNum (ThisReassignmentError)                         = Just 47
-- toHashNum (ImmutableVariableError _)                      = Just 48
-- toHashNum (PatternArityMismatchError _ _ _)               = Just 49
-- toHashNum (PatternTypeMismatchError _ _)                  = Just 50
-- toHashNum (NonMaybeExtractorPatternError _)               = Just 51
-- toHashNum (InvalidPatternError _)                         = Just 52
-- toHashNum (InvalidTupleTargetError _ _ _)                 = Just 53
-- toHashNum (InvalidTupleAccessError _ _)                   = Just 54
-- toHashNum (CannotReadFieldError _)                        = Just 55
-- toHashNum (NonAssignableLHSError)                         = Just 56
-- toHashNum (ValFieldAssignmentError _ _)                   = Just 57
-- toHashNum (UnboundVariableError _)                        = Just 58
-- toHashNum (BuriedVariableError _)                         = Just 59
-- toHashNum (ObjectCreationError _)                         = Just 60
-- toHashNum (NonIterableError _)                            = Just 61
-- toHashNum (EmptyArrayLiteralError)                        = Just 62
-- toHashNum (NonIndexableError _)                           = Just 63
-- toHashNum (NonSizeableError _)                            = Just 64
-- toHashNum (FormatStringLiteralError)                      = Just 65
-- toHashNum (UnprintableExpressionError _)                  = Just 66
-- toHashNum (WrongNumberOfPrintArgumentsError _ _)          = Just 67
-- toHashNum (UnaryOperandMismatchError _ _)                 = Just 68
-- toHashNum (BinaryOperandMismatchError _ _ _ _)            = Just 69
-- toHashNum (UndefinedBinaryOperatorError _)                = Just 70
-- toHashNum (NullTypeInferenceError)                        = Just 71
-- toHashNum (CannotBeNullError _)                           = Just 72
-- toHashNum (TypeMismatchError _ _)                         = Just 73
-- toHashNum (TypeWithCapabilityMismatchError _ _ _)         = Just 74
-- toHashNum (TypeVariableAmbiguityError _ _ _)              = Just 75
-- toHashNum (FreeTypeVariableError _)                       = Just 76
-- toHashNum (TypeVariableAndVariableCommonNameError _)      = Just 77
-- toHashNum (UnionMethodAmbiguityError _ _)                 = Just 78
-- toHashNum (MalformedUnionTypeError _ _)                   = Just 79
-- toHashNum (RequiredFieldMutabilityError _ _)              = Just 80
-- toHashNum (ProvidingTraitFootprintError _ _ _ _)          = Just 81
-- toHashNum (TypeArgumentInferenceError _ _)                = Just 82
-- toHashNum (AmbiguousTypeError _ _)                        = Just 83
-- toHashNum (UnknownTypeUsageError _ _)                     = Just 84
-- toHashNum (AmbiguousNameError _ _)                        = Just 85
-- toHashNum (UnknownNamespaceError _)                       = Just 86
-- toHashNum (UnknownNameError _ _)                          = Just 87
-- toHashNum (ShadowedImportError _)                         = Just 88
-- toHashNum (WrongModuleNameError _ _)                      = Just 89
-- toHashNum (BadSyncCallError)                              = Just 90
-- toHashNum (PrivateAccessModifierTargetError _)            = Just 91
-- toHashNum (ClosureReturnError)                            = Just 92
-- toHashNum (ClosureForwardError)                           = Just 93
-- toHashNum (MatchMethodNonMaybeReturnError)                = Just 94
-- toHashNum (MatchMethodNonEmptyParameterListError)         = Just 95
-- toHashNum (ImpureMatchMethodError _)                      = Just 96
-- toHashNum (IdComparisonNotSupportedError _)               = Just 97
-- toHashNum (IdComparisonTypeMismatchError _ _)             = Just 98
-- toHashNum (ForwardInPassiveContext _)                     = Just 99
-- toHashNum (ForwardInFunction)                             = Just 100
-- toHashNum (ForwardTypeError _ _)                          = Just 101
-- toHashNum (ForwardTypeClosError _ _)                      = Just 102
-- toHashNum (CannotHaveModeError _)                         = Just 103
-- toHashNum (ModelessError _)                               = Just 104
-- toHashNum (ModeOverrideError _)                           = Just 105
-- toHashNum (CannotConsumeError _)                          = Just 106
-- toHashNum (CannotConsumeTypeError _)                      = Just 107
-- toHashNum (ImmutableConsumeError _)                       = Just 108
-- toHashNum (CannotGiveReadModeError _)                     = Just 109
-- toHashNum (CannotGiveSharableModeError _)                 = Just 110
-- toHashNum (NonValInReadContextError _)                    = Just 111
-- toHashNum (NonSafeInReadContextError _ _)                 = Just 112
-- toHashNum (NonSafeInExtendedReadTraitError _ _ _)         = Just 113
-- toHashNum (ProvidingToReadTraitError _ _ _)               = Just 114
-- toHashNum (SubordinateReturnError _ _)                    = Just 115
-- toHashNum (SubordinateArgumentError _)                    = Just 116
-- toHashNum (SubordinateFieldError _)                       = Just 117
-- toHashNum (ThreadLocalFieldError _)                       = Just 118
-- toHashNum (ThreadLocalFieldExtensionError _ _)            = Just 119
-- toHashNum (ThreadLocalArgumentError _)                    = Just 120
-- toHashNum (PolymorphicArgumentSendError _ _)              = Just 121
-- toHashNum (PolymorphicReturnError _ _)                    = Just 122
-- toHashNum (ThreadLocalReturnError _ _)                    = Just 123
-- toHashNum (MalformedConjunctionError _ _ _)               = Just 124
-- toHashNum (CannotUnpackError _)                           = Just 125
-- toHashNum (CannotInferUnpackingError _)                   = Just 126
-- toHashNum (UnsplittableTypeError _)                       = Just 127
-- toHashNum (DuplicatingSplitError _)                       = Just 128
-- toHashNum (StackboundArrayTypeError _)                    = Just 129
-- toHashNum (ManifestConflictError _ _)                     = Just 130
-- toHashNum (ManifestClassConflictError _ _)                = Just 131
-- toHashNum (UnmodedMethodExtensionError _ _)               = Just 132
-- toHashNum (ActiveTraitError _ _)                          = Just 133
-- toHashNum (NewWithModeError)                              = Just 134
-- toHashNum (UnsafeTypeArgumentError _ _)                   = Just 135
-- toHashNum (OverlapWithBuiltins)                           = Just 136
-- toHashNum (SimpleError _)                                 = Just 137
-- toHashNum (ReverseBorrowingError)                         = Just 138
-- toHashNum (BorrowedFieldError _)                          = Just 139
-- toHashNum (LinearClosureError _ _)                        = Just 140
-- toHashNum (BorrowedLeakError _)                           = Just 141
-- toHashNum (NonBorrowableError _)                          = Just 142
-- toHashNum (ActiveBorrowError _ _)                         = Just 143
-- toHashNum (ActiveBorrowSendError _ _)                     = Just 144
-- toHashNum (DuplicateBorrowError _)                        = Just 145
-- toHashNum (StackboundednessMismatchError _ _)             = Just 146
-- toHashNum (LinearCaptureError _ _)                        = Just 147
-- toHashNum _ = Nothing







-- explainErr :: Int -> String
-- explainErr 1 =
-- explainErr 2 =
-- explainErr 3 =
-- explainErr 4 =
-- explainErr 5 =
-- explainErr 6 =
-- explainErr 7 =
-- explainErr 8 =
-- explainErr 9 =
-- explainErr 10 =
-- explainErr 11 =
-- explainErr 12 =
-- explainErr 13 =
-- explainErr 14 = "You seem to have lost you main class,\nthat is needed if you want to make an executable program."
-- explainErr 15 =
-- explainErr 16 =
-- explainErr 17 =
-- explainErr 18 =
-- explainErr 19 =
-- explainErr 20 =
-- explainErr 21 =
-- explainErr 22 =
-- explainErr 23 =
-- explainErr 24 =
-- explainErr 25 =
-- explainErr 26 =
-- explainErr 27 =
-- explainErr 28 =
-- explainErr 29 =
-- explainErr 30 =
-- explainErr 31 =
-- explainErr 32 =
-- explainErr 33 =
-- explainErr 34 =
-- explainErr 35 =
-- explainErr 36 =
-- explainErr 37 =
-- explainErr 38 =
-- explainErr 39 =
-- explainErr 40 =
-- explainErr 41 =
-- explainErr 42 =
-- explainErr 43 =
-- explainErr 44 =
-- explainErr 45 =
-- explainErr 46 =
-- explainErr 47 =
-- explainErr 48 =
-- explainErr 49 =
-- explainErr 50 =
-- explainErr 51 =
-- explainErr 52 =
-- explainErr 53 =
-- explainErr 54 =
-- explainErr 55 =
-- explainErr 56 =
-- explainErr 57 =
-- explainErr 58 =
-- explainErr 59 =
-- explainErr 60 =
-- explainErr 61 =
-- explainErr 62 =
-- explainErr 63 =
-- explainErr 64 =
-- explainErr 65 =
-- explainErr 66 =
-- explainErr 67 =
-- explainErr 68 =
-- explainErr 69 =
-- explainErr 70 =
-- explainErr 71 =
-- explainErr 72 =
-- explainErr 73 =
-- explainErr 74 =
-- explainErr 75 =
-- explainErr 76 =
-- explainErr 77 =
-- explainErr 78 =
-- explainErr 79 =
-- explainErr 80 =
-- explainErr 81 =
-- explainErr 82 =
-- explainErr 83 =
-- explainErr 84 =
-- explainErr 85 =
-- explainErr 86 =
-- explainErr 87 =
-- explainErr 88 =
-- explainErr 89 =
-- explainErr 90 =
-- explainErr 91 =
-- explainErr 92 =
-- explainErr 93 =
-- explainErr 94 =
-- explainErr 95 =
-- explainErr 96 =
-- explainErr 97 =
-- explainErr 98 =
-- explainErr 99 =
-- explainErr 100 =
-- explainErr 101 =
-- explainErr 102 =
-- explainErr 103 =
-- explainErr 104 =
-- explainErr 105 =
-- explainErr 106 =
-- explainErr 107 =
-- explainErr 108 =
-- explainErr 109 =
-- explainErr 110 =
-- explainErr 111 =
-- explainErr 112 =
-- explainErr 113 =
-- explainErr 114 =
-- explainErr 115 =
-- explainErr 116 =
-- explainErr 117 =
-- explainErr 118 =
-- explainErr 119 =
-- explainErr 120 =
-- explainErr 121 =
-- explainErr 122 =
-- explainErr 123 =
-- explainErr 124 =
-- explainErr 125 =
-- explainErr 126 =
-- explainErr 127 =
-- explainErr 128 =
-- explainErr 129 =
-- explainErr 130 =
-- explainErr 131 =
-- explainErr 132 =
-- explainErr 133 =
-- explainErr 134 =
-- explainErr 135 =
-- explainErr 136 =
-- explainErr 137 =
-- explainErr 138 =
-- explainErr 139 =
-- explainErr 140 =
-- explainErr 141 =
-- explainErr 142 =
-- explainErr 143 =
-- explainErr 144 =
-- explainErr 145 =
-- explainErr 146 =
-- explainErr 147 =
-- explainErr _ = ""