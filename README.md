# Ribbon Language

## Introduction

The start of the Ribbon Computer Language which will be at ribbon-lang.org.

The language translates from Ribbon .rbb files to Idris and then translates to Typescript.

Some features of Ribbon are:
* Keyboard typing is from left to right without interruptions, as in English language typing
* Indentation not allowed, indentation has no meaning
* Variable names can be multiple words, eg ```all lengths``` translates to ```allLengths```
* Sentence lines can be grouped into paragraphs

## Code sample

The following in Ribbon .rbb file.

    average is string to double
    average of str is cast total length / cast num words

    total length is sum of all lengths of words str
    num words is word count of str

    word count is string to nat
    word count of str is the length of words of str

    all lengths of strs is map lengths strs

    show average is string to string
    show average of str is "The average word length is:" ++
    show the average of str ++ "\n"""".stripMargin

Translates to the equivalent Idris, which would then be translated to Typescript (not given here).
    
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


