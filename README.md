# HaskellPool
Epitech Pool of Functionnal programming.

This repo contain my Haskell code for the EPITECH Haskell Pool.
It's a week to discover functionnal programming.

This week was composed of 2 days to discover basics, by coding higher order function like map, filter, or even fold.

##PushSwap_Checker##

The end of the week was devoted to the "PushSwap_Checker", a mini-haskell project.

The Checker should take a list of int as his args, and a set of action to apply on the list (for exemple "swap the 2 first elem")
on the standard entry.

It will apply the actions to the list and print "OK" if it sort the list, "KO" otherwise.

This checker can treat 100k actions in 8 seconds if interpreted, or in 4 if compiled.

##How to use##

Either runhaskell (runhaskell PushSwap_Checker 1 3 2) or ghc

Once started, the program await for a set of action

Test file medium contain 100k of the possibles actions.
