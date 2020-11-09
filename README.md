# Ribbon Language

<img src="./ribbon.png" style="width:113px">

## Introduction

The experimental Ribbon Computer Language at [ribbon-lang.org](https://ribbon-lang.org/), this project is very early in development and the syntax is in flux.

This translates from Ribbon .rbb files to Idris and then translates to Typescript. Idris is an amazing language, this is really just a more simplified Idris with some other ideas added.

Some features of Ribbon are:
* Keyboard typing is from left to right without interruptions, as in English language and you should be able to program this language by speaking verbally although this is not a main language goal
* Indentation not allowed, indentation has no meaning
* Variable names can be multiple words, eg ```all lengths``` is allowed and translates to ```allLengths```
* Sentence lines can be grouped into paragraphs, a paragraph defines a common scope for variable names. Functions can be referenced from within and outside of the paragraph scope
* Functions can only take one parameter and return one result, if you want a function with multiple parameters then create a data type which encapsulates the parameters
* Use of [Lojban](https://en.wikipedia.org/wiki/Lojban) words are encouraged for the definition of functions
* Resultant code should be human readable but still functional programming style

## Why?

The features of the language are unusual, this is partially an experiment and my personal opinion of what a good computer language should be.

## How?

How does it compile? It does not compile, it translates from Ribbon to Idris to Typescript in two steps. Since one language is simular to another in some way, translation from language to language can be done. The purpose of translation from Ribbon to Idris is to build upon the features that the Idris language provides, if the code compiles in Idris then it will be OK in Typescript.

Typescript allows code to deploy to multiple environments whether it is within NodeJS or React or other.

## Code sample

The following in Ribbon .rbb file.

```
average is string to double
cast total length / cast num words
total length is sum of all lengths of words of
num words is word count of

word count is string to nat
the length of words of

all lengths is list string to list nat
map lengths of

show average is string to string
display is "The average word length is:"
display ++ show the average of ++ "\n"
```

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

### Reserved words

is, of, to, for

#### is reserved word

```is```, within the context of a function definition means that the parameter types follow as input type to output type

An example is ```average is string to double``` which is the same as the following in typescript.

```typescript
function average(str: string): number { 
  // Implementation not yet defined
}
```

#### of reserved word

@todo

#### to reserved word

```to``` has two use cases, each within a different context.

For the first use case ```to``` is used within the function definition to indiciate the function takes an input parameter and returns another output.

An example is ```all lengths is list string to list nat``` which would result in typescript as such.

```typescript
function allLengths(s: string[]): number[] {
    // Implementation not yet define
 }
```

The second use case of ```to``` will appear after the keyword ```is```, it is for assignment of data and must precede ```for```. The ```for``` keyword indicates which value to return.

```update manager name is name to manager name for manager```

The equivalent typescript.

```typescript
interface Manager {
  name: string;
}

interface Update {
  name: string;
  manager: Manager;
}

function update(update: Update): Manager {
    update.manager.name = update.name;
    return update.manager;
}
```

#### for reserved word

The for keyword usage is defined in the previous ```to``` section.

## Representing data

Representation of data as relationships between entities is supported within the language and querying those relationships is also supported. Most other computer
  languages provide data representation and query through a database API or library, in this case it is built into the language.

The intent here is to provide an in-language feature for data representation and query which can then be attached to different database backends in a transparent way for the programmer. The programmer should not need to know if they are using a GraphQL or Graph database or MySql or in memory.

### Data schema

```javascript
// Relationships are sentences with cardinality on words
1..1 manager supervises 0..n staff
0..n staff is managed by 1..1 manager
the staff are manager and employee
```

```javascript
// Data is like a function but without a return type
name is string // Name is defined as a data type with type of string
age is number // Age is a data type with type of number
gender is male or female // Union type (enum)
staff is name age gender // Product type
```

### Data instance creation

```javascript
philip is name 'Philip' age 45 gender male
fred is name 'Fred' age 40 gender male
fred supervises philip
```

### Querying data

```javascript
// Here manager will take the value of fred from the previous Data instance creation
manager? supervises philip
```
### Function with multiple parameters combined into one parameter to allow for update

```javascript
name is string
update is name manager // update is a data type combining name and manager
update manager name is update to manager // function goes from data type update to data type manager
update manager name of update is update.name to update.manager.name for update.manager
```

## Build

This example program uses seed to build.
https://github.com/tindzk/seed

Run bloop server then.

`seed bloop`

`seed build idrisparser:jvm`

`seed run idrisparser:jvm`

Generate IntelliJ Idea project.

`seed idea`


```
Notes

record Person where
    constructor MkPerson
    firstName, middleName, lastName : String
    age : Int

fred : Person
fred = MkPerson "Fred" "Joe" "Bloggs" 30

*Record> firstName fred
"Fred" : String
*Record> age fred
30 : Int

record { firstName = "Jim" } fred

record { firstName = "Jim", age $= (+ 1) } fred
```


