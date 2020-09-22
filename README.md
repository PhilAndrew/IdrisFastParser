# Ribbon Language

## Introduction

The experimental Ribbon Computer Language at [ribbon-lang.org](https://ribbon-lang.org/), this project is very early in development.

This translates from Ribbon .rbb files to Idris and then translates to Typescript.

Some features of Ribbon are:
* Keyboard typing is from left to right without interruptions, as in English language and you should be able to program this language by speaking verbally although this is not a main language goal
* Indentation not allowed, indentation has no meaning
* Variable names can be multiple words, eg ```all lengths``` is allowed and translates to ```allLengths```
* Sentence lines can be grouped into paragraphs, a paragraph defines a common scope for variable names. Functions can be referenced from within and outside of the paragraph scope
* Functions can only take one parameter and return one result, if you want a function with multiple parameters then create a data type which encapsulates the parameters
* Use of [Lojban](https://en.wikipedia.org/wiki/Lojban) words are encouraged for the definition of functions

## Why?

The features of the language are unusual, this is partially an experiment and my personal opinion of what a good computer language should be.

## How?

How does it compile? It does not compile, it translates from Ribbon to Idris to Typescript in two steps. Since one language is simular to another in some way, translation from language to language can be done. The purpose of translation from Ribbon to Idris is to build upon the features that the Idris language provides, if the code compiles in Idris then it will be OK in Typescript.

Typescript allows code to deploy to multiple environments whether it is within NodeJS or React or other.

## Code sample

The following in Ribbon .rbb file.

    average is string to double
    average of str is cast total length / cast num words
    total length is sum of all lengths of words of str
    num words is word count of str

    word count is string to nat
    word count of str is the length of words of str

    all lengths is list string to list nat
    all lengths of strs is map lengths strs

    show average is string to string
    display is "The average word length is:"
    show average of str is display ++ show the average of str ++ "\n"

Translates to the equivalent Idris, which would then subsequently be translated to Typescript (Typescript not given here).
    
    average : (str: String) -> Double
    average str = let numWords = wordCount str
                totalLength = sum (allLengths (words str)) in
                cast totalLength / cast numWords
                where
                wordCount : String -> Nat
                wordCount str = length (words str)
    
    allLengths : List String -> List Nat
    allLengths strs = map lengths strs
    
    showAverage : String -> String
    showAverage str = "The average word length is: " ++
    show (average str) ++ "\n"


## Build

This example program uses seed to build.
https://github.com/tindzk/seed

It is not necessary to use seed, sbt or any other build tool could be used.

If you choose to continue to use seed then you need to run the boop server in another terminal then do the following.


`seed bloop`

`seed build idrisparser:jvm`

`seed run idrisparser:jvm`

Generate IntelliJ Idea project.

`seed idea`


