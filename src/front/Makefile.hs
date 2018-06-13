
module Makefile where

import Data.Text.Prettyprint.Doc

import Data.String.Utils

import Prelude hiding(all)

($$) s e = s <> hardline <> e
($\$) t ts = (t <> hardline <> hardline <> ts)
tab = pretty "\t"
all = pretty "all"
bench = pretty "bench"
clean = pretty "clean"
rm args = (pretty "rm -rf" <+> hsep args)
phony = pretty ".PHONY"
cc args = (pretty "$(CC)" <+> hsep args)
flags = pretty "$(FLAGS)"
benchFlags = pretty "$(BENCH_FLAGS)"
target = pretty "$(TARGET)"
inc = pretty "$(INC)"
lib = pretty "$(LIB)"
deps = pretty "$(DEPS)"
defs = pretty "$(DEFINES)"
dSYM = pretty ".dSYM"
i = (pretty "-I" <+>)
o = (pretty "-o" <+>)
parent = pretty ".."

generateMakefile :: [String] ->
    String -> String -> String -> String -> String -> String -> Doc ann
generateMakefile classFiles progName compiler ccFlags incPath defines libs =
    decl "CC" [compiler]
    $$
    decl "TARGET" [progName]
    $$
    decl "INC" [incPath]
    $$
    decl "LIB" [libs]
    $$
    decl "FLAGS" [ccFlags]
    $$
    decl "BENCH_FLAGS" [replace "-ggdb" "-O3" ccFlags]
    $$
    decl "DEFINES" [defines]
    $$
    decl "DEPS" ("shared.c" : classFiles)
    $\$
    noCmdRule all target
    $\$
    rule target deps
        (cc [flags, i inc, i parent, deps, lib, lib, defs, o target])
    $\$
    rule bench deps
        (cc [benchFlags, i inc, i parent, deps, lib, lib, defs, o target])
    $\$
    rule clean emptyDoc
        (rm [target, target <> dSYM])
    $\$
    noCmdRule phony (all <+> bench <+> clean)
    where
        decl var rhs = pretty var <> equals <> hsep (map pretty rhs)
        rule target deps cmd = target <> colon <+> deps $$
                                tab <> cmd
        noCmdRule target deps = target <> colon <+> deps
