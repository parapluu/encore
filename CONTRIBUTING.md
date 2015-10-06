# Contributing

Thanks for contributing to the Encore programming language.  
Here are some guidelines for making the most of your contribution.

## Opening issues

If you find a bug, please feel free to open an issue describing it.  
Try to present the bug in terms of the smallest program exhibiting that bug.

If you taking the time to mention a problem, even a seemingly minor one, it is
greatly appreciated, and a totally valid contribution to this project. Thank
you!

## Git

Please do **not** push to the `master` branch. Rather, clone the
encore repository in your own github account, push there and send a pull
request. This allows for code to be reviewed before it gets into `master`. Also,
when implementing a large feature, please use a feature branch.

## Guidelines (taken from [here](http://www.booleanknot.com/blog/2013/09/07/pull-requests.html))

### Stay on target

A pull request should be focused on a single bug fix or feature.

Resist the temptation **to clean up whitespace**, or **fix formatting errors**, or **add
useful rules to .gitignore**. Don't change the version number, either; is not your job
to determine release cycles. If you must do any of those things, send the author
a separate pull request.

**Diffs that are polluted with whitespace and formatting fixes are harder to
understand**, and pull requests that implement multiple features are harder to
test and approve.

**Stay focused**. Make things easy for the project maintainer, and they'll get your pull
request merged much more quickly.

### Present a clean history
The commits in your pull request should present a clean
and easily understandable history. The project maintainer won't want to see commits,
or all your reverts and mistakes. All of that just obscures the meaning of the
code.

Opinion on this differs a little between developers, but my preference is that
you should clean up your branch an interactive `git rebase` before you submit your
pull request.

Your goal shouldn't be to give the project maintainer an accurate, chronological history
of your changes, but to present a history that aids their understanding.

### Understand git commit messages

The first line of a git commit message is special. It's used to provide a brief
description of the commit. Think of it like the subject line of an email.

This summary line should be short. Opinions differ on how short; some recommend
it should be as short as 50 characters, while others recommend no more than 70
to 75 characters. Github itself truncates summaries longer than 69 characters so
is the limit I tend to use.

### Include tests

If the project has automated tests, and it makes sense to do so, include tests
in your pull request. This not only prevents regressions, but it also gives the
project maintainer confidence we actually spent time making sure your code
works.

If you submit a pull request without tests, to a project with tests, you're
guaranteed to get a grumpy project maintainer telling you to go away and write
some.

### Code formatting

We have not been enforcing any rules before, but we are now. All new
Haskell code has to adhere to these conventions. No excuses.

 - No snake cases anywhere. We use camel case.
 - Type signatures on top level functions.
 - We strongly recommend using `hlint`, but without the "Eta reduce" warning (run `hlint --ignore="Eta reduce" File.hs`).

#### Indentation

##### Bad
```
foo = do ..
         ..
         return ()
```
##### Good

```
foo = do
  ..
  ..
  return ()
```

##### Rationale

If the name of the function changes, in the bad version you need to
change all code in the body, too. This hides who has actually written
the code (and why). *This also holds for `case` expressions (do a line
break after `of`) and `if-then-else`.* When implementing a function,
the same might apply: starting the implementation in the same line
might cause git history problems. However, for functions, this choice
remains up to the programmer.
