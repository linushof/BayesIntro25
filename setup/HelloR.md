
# Prerequisites

- [x] [R](https://cloud.r-project.org) installed
- [x] [RStudio](https://posit.co/products/open-source/rstudio/)
  installed

# Hello `R`

## What is R code and how does it work?

`R` is an *interpreted high-level* programming language specifically
tailored to statistical computing. In fact, `R` can do more than
statistical computing, but that’s what it’s designed for, that’s what
it’s good at, and that’s what we’re primarily concerned with in this
course.

*High level* refers to the fact that we as `R` users aren’t required to
know anything at all about the computer’s architecture and machine code.
This doesn’t mean that machine code isn’t important. Machine code is the
only language that your computer understands. However, machine code is
hard to write and not a desirable tool to actively work with in data
analysis projects. `R` code makes your lives much easier: It is much
more readable and straightforward to write because it relies on elements
of natural language, programming concepts such as loops and functions,
data structures like data frames, and expressions including arithmetic
and true/false statements, which you’re either already familiar with or
which you’ll easily learn.

Because `R` code is not machine code, which is ultimately required to
get your machine do the work, `R` code needs to be interpreted and
translated to machine code. This is what a so-called *interpreter* does
and this is what makes `R` an *interpreted* language. If you are just
concerned with data analysis, it’s not necessary to understand the
technical aspects of this translation process. What you need to learn
and practice is how write `R` code.

`R` code consists of written commands that are read and executed by a
command-line interpreter. To access the interpreter and write `R` code,
we take a brief look into the *R console*, `R`’s command-line interface.

<img src="screenshots/console.png" data-fig-align="center" />

In the console you can write code (e.g., `1+1`) and execute it by
pressing `Enter`. Below the code, `R` returns the *output* (i.e., `2`).
Every line of code needs to end up in the console in order to be
executed and to obtain its output.

## Giving R a better look with RStudio

The console has some shortcomings. For one, the console doesn’t secure
your code once you close it. Since your projects can easily take days,
months, or even years, this is a problem: You won’t and can’t start from
scratch every time. The simple solution is to write your code in a text
file that can be saved on your machine and opened again once you need
it. Text files that only contain code are referred to as *scripts* and
usually have a file extension that indicates the language in which the
code is written. `R` scripts have the extension ‘.R’.

Another shortcoming of the console is that it doesn’t allow for much
more than just writing code. However, throughout this course you’ll
realize that a proper data analysis workflow requires much more than
just writing code. Thus, we’ll not directly work in console but rely on
*RStudio*. RStudio is an *integrated development environment* (IDE) that
provides a powerful and user-friendly interface for working with `R`.

<img src="screenshots/RStudio.png" data-fig-align="center" />

RStudio is designed to ease the use of `R` and improve your efficiency
by offering a range of tools and features that streamline data
management, coding, and analysis.

One feature of RStudio is that it offers more than one *panel*. Each
panel provides access to different kinds of information. E.g., the upper
left panel in the above screenshot entails a script `script.R`, in which
we can write and secure the code we previously entered directly in the
console. In the panel below, you find the console. Remember that the
console remains the processing engine and that every line of code must
be passed through it in order to be executed. However, you don’t have to
copy-and-paste your code from the script to the console every time.
Instead, you can directly execute code from the script in the console by
pressing the key combination `ctrl` + `enter` (Windows) or `cmd` +
`enter` (macOS). You can store this script like any other file on your
computer, ready to be executed at any time.

You’ll learn about the other panels and their functionality on the fly
throughout the course. At this point, let’s just briefly consider some
more advantages of working in RStudio:

- Code writing: RStudio provides a range of features such as syntax
  highlighting, code completion, and error checking that help you to
  write and debug `R` code.

- Documentation & help: RStudio provides easy access to `R`’s
  documentation and help pages, making it easier to learn which commands
  and functions to use in order to solve a coding task.

- Workspace management: RStudio provides a range of tools for managing
  your project files and *R workspace*, including the ability to view,
  load, and save data, and to manage *R packages*.

- Integration of version control systems: RStudio integrates with
  popular version control systems such as Git/GitHub, making it easier
  to manage and collaborate on R projects.

You’ll learn about these features and how to integrate them in your
workflow throughout this course. The important take-away is that
everything that can be done using only the console can be done in
RStudio, too, but better. Thus, from now on, we will only interact with
`R` via RStudio.

## Learning the R basics

Now that we’ve set up `R`, we want to learn the basics of programming in
`R`. Many commands, functions, and concepts that we use in this course
will be introduced as we actually need them. Here, we start by focusing
on the most essential points to give you an idea of how `R` works and
what it feels like to create and manipulate simple data structures.

### Arithmetic Expressions

*Arithmetic expressions* trigger mathematical operations on numerical
data and you probably know them since elementary school. In `R`, these
expressions follow the same order of operations as in standard
mathematics, even if some of the *operators* may look a bit different
from the ones on paper or in the calculator.

``` r
1+1 # addition
```

    [1] 2

``` r
1-1 # subtraction
```

    [1] 0

``` r
1*1 # multiplication
```

    [1] 1

``` r
1/1 # division
```

    [1] 1

``` r
# expressions where order of operations matter
1+1/2 #  division prior addition
```

    [1] 1.5

``` r
(1+1)/2 # parentheses first
```

    [1] 1

### Objects

`R` is a *object-oriented* language because we use arithmetic
expressions and all kinds of other functions to create and manipulate
*objects*. Objects can be thought of as data storage, which we assign
names to. More specifically, objects usually entail data that is
structured in some way. Based on what exactly this data is and how it is
structured, we refer to the object as a certain *object type*.
Irrespective of the object type, we can give objects whatever name we
want. We now look at some of the most common object types.

#### Scalars

Scalars are objects that represent individual values. They can take on
different data types, such as numerical, character, string, or logical:

- Numerical scalars represent numeric values, such as integers
  (`...,-2,1,0,1,2, ...`) or decimal numbers.

- Character scalars represent individual characters, such as letters,
  digits, or punctuation marks.

- String scalars represent a sequence of characters, often in the form
  of a word or a phrase.

- Logical scalars represent logical values, such as TRUE or FALSE, which
  are commonly used for conditional expressions or comparisons.

We assign names to scalars — and all other objects — by using the
*assignment operator* `<-` (Windows shortcut: `Alt` + `-`; macOS:
`Option` + `-`).

``` r
scal_num <- 1
scal_chr <- "a"
scal_str <- "word"
scal_log <- TRUE # logical/Boolean
```

We can access (look into) the object by typing its name and sending the
line to the console. Remember how we send code from the script to the
console: Go to the respective code line and press the key combination
`ctrl` + `enter` (Windows) or `cmd` + `enter` (macOS).

``` r
scal_num
```

    [1] 1

Having created scalar objects and knowing what they are, we can continue
operating on them. Let’s run some arithmetic expressions again.

``` r
scal_num + 1
```

    [1] 2

``` r
scal_num + scal_num
```

    [1] 2

``` r
scal_num + scal_num / 2
```

    [1] 1.5

``` r
(scal_num + scal_num) / 2
```

    [1] 1

The above examples gives us an idea of what `R` is doing when it is
supplied with an object that we previously created: It takes the data
that is stored inside them and runs the operation on the data.

Storing data in objects becomes particularly helpful if the data is not
just a scalar. Before we turn to more complex object types, let’s
briefly consider how we can overwrite existing objects or create new
objects by running operations on existing ones.

``` r
scal_num <- scal_num + 1  # overwriting
scal_num
```

    [1] 2

``` r
scal_num2 <- scal_num + 1 # creating new objects
scal_num2
```

    [1] 3

Occasionally, we’ll write code that represents an invalid operation. In
these cases, `R` will return an error message. E.g., we can multiply
numerical values, but not characters.

``` r
scal_chr * 4
```

<span style="color:red;">Error in scal_chr \* 4 : non-numeric argument
to binary operator</span>

If `R` returns an error, we have to inspect the code and find the error.
In most cases, looking up the error on the internet will help you to
solve the issue; simply re-running the command that triggered the error
message will most likely not.

#### Vectors

When working in `R`, vectors are a data structure you’ll commonly
encounter. A vector is an ordered collection of single values (ordered
in terms of position within the vector not magnitude of value). Although
not mandatory, values of a vector often belong to the same data type
(e.g., numeric or character or logical). We can produce a vector object
using the familiar assignment operator `<-` and the `c()` command, which
combines multiple values into a vector. (c for ‘’combine’’ or
‘’concatenate’’.)

``` r
x <- c(1, 2, 3, 4) # create a vector object x
x
```

    [1] 1 2 3 4

One of `R`’s prime features is it’s *vectorized* nature. This means that
`R` performs the same operation on an entire vector — i.e., on each
value within a vector — at once.

``` r
x*2
```

    [1] 2 4 6 8

When manipulating vectors in `R`, the position of each value within the
vector is taken into account. Specifically, operations between vectors
are performed in a way that applies a given operation to elements that
occupy the same position in their vectors. To illustrate, let’s multiply
two vectors `x` and `y` of the same length (both contain 4 values). The
multiplication operation will be applied to the first value of `x` and
the first value of `y`, the second value of x and the second value of y,
and so on. The resulting output will be a vector of the same length as
`x` and `y`.

``` r
x <- c(1, 2, 3, 4) # vector x
y <- c(1, 2, 3, 4) # vector y
x*y
```

    [1]  1  4  9 16

`R`s vectorized nature is a bit more tricky to deal with if vectors
don’t have the same length. Consider the following example where the `x`
from above (4 values) is multiplied with a shorter vector `z` (2
values).

``` r
z <- c(1,2) # vector z
x*z
```

    [1] 1 4 3 8

What’s the trick here? When two vectors don’t have the same length, the
shorter vector is implicitly repeated until there are as many values as
in the longer vector. In the above case, `z` is implicitly translated
from `c(1,2)` into `c(1,2,1,2)`, i.e., it’s replicated such that we
obtain 4 values. Then, as previously, the operations are performed
element wise. We can check this behavior by replicating the vector `z`
manually.

``` r
z <- rep(z, 2) # replicates z twice and overwrite
z
```

    [1] 1 2 1 2

``` r
x*z
```

    [1] 1 4 3 8

There is a lot more to learn about vectors in this course. However, for
now, we turn to a bit more complex data structure, one which entails
multiple vectors.

#### Data Frames

Being a collection of values, vectors are ideally suited to represent
variables in a data frame. Data frames are a data structures you’ll
encounter in virtually every data analysis project. [According to Hadley
Wickham](https://r4ds.had.co.nz/tidy-data.html), every messy data set is
messy in its own way, but tidy data sets are all alike. Specifically,
tidy data sets follow a small set of simple rules:

1.  Each variable must have its own column.
2.  Each observation must have its own row.
3.  Each value must have its own cell.

To put these rules into action, consider the following example: First,
we create 3 individual vectors, `x`, `y`, `z`, and then represent them
as different variables in a data frame object `dat`. For the latter
operation, we use the function `data.frame()`, which simply takes all
vectors as inputs.

``` r
x <- c(1, 2, 3)
y <- c(4, 5, 6)
z <- c(7, 8, 9)
dat <- data.frame(x, y, z)
dat
```

      x y z
    1 1 4 7
    2 2 5 8
    3 3 6 9

Not all data sets in the ‘’wild’’ look this neat right from the start
and usually they will be a lot bigger. However, irrespective of its
initial form and size, in most data analysis projects you want to bring
the data into this format before you start analyzing it.

One advantage becomes immediately clear: Columns represent
variables/vectors and row numbers indicate positions within the vectors.
To grasp the full advantage, assume that each row represents a single
unit of analysis (e.g., countries, companies, individuals) — this is
what rows usually do. Now we can see that each value on each variable
belongs to exactly one unit, each indicated by the row number. To make
this clear, let’s add an indicator variable `ID` to `dat`.

``` r
ID <- c("A", "B", "C")
dat <- data.frame(ID, dat)
dat
```

      ID x y z
    1  A 1 4 7
    2  B 2 5 8
    3  C 3 6 9

Now we have three different analysis units, `A`, `B`, `C`, which could
stand for individual countries, companies, people or the like, each of
which has one value on the variables `x`, `y`, and `z`. In many —
although not all — cases we want to operate on variables across all
analysis units (e.g., computing a variable’s mean, recoding values
within variables). In this case, we can make optimal use of `R`’s
vectorized nature and simply apply the same operation on a variable to
all rows/units at once. You’ll soon appreciate both this particular data
frame format.

As data frames are a very common data structure, we need to learn a set
of basic commands that help us to manipulate data frames and obtain the
information we want. Throughout the course, we’ll also learn about a set
of functions from the `tidyverse` package, which make writing code and
working with data frames particularly easy, even if the actual
manipulations are getting more complex.

As with vectors, we can apply the same operation to an entire data
frame. Consider the following example, where each cell entry is
multiplied by 2.

``` r
x <- c(1, 2, 3)
y <- c(4, 5, 6)
z <- c(7, 8, 9)
dat <- data.frame(x, y, z)
dat*2
```

      x  y  z
    1 2  8 14
    2 4 10 16
    3 6 12 18

To multiply each single value (cell entry), `R` operates element-wise
again, starting from the left and traversing the columns one by one from
top to bottom. This element-wise operation is implied by `R`’s
vectorized nature. Implicitly, `R` translates all cell entries into a
single vector and repeats the scalar `2` as many times as there are cell
entries.

Rarely, we want to apply the same operation to all values in a data
frame at once. Instead, we usually want to apply operations to specific
variables/columns/vectors in a data frame. There are multiple ways to do
so. First, we learn about how to retrieve a variable from a data frame.
In the following, you can see at least 3 ways that come with the
standard installation of `R`.

``` r
dat
```

      x y z
    1 1 4 7
    2 2 5 8
    3 3 6 9

``` r
dat$x # dollar notation with variable name
```

    [1] 1 2 3

``` r
dat[, 1] # matrix notation with column number
```

    [1] 1 2 3

``` r
dat[, "x"] # matrix notation with variable name
```

    [1] 1 2 3

In the *matrix notation*, values prior the comma represent rows and
values after the comma represent columns. The notation has the advantage
we can retrieve multiple variables and rows at once. In the following, I
use a vector `c(1,2)` after the comma to indicate that I want to
retrieve the first and second column from the data set.

``` r
# multiple columns
dat[, c(1,2)]
```

      x y
    1 1 4
    2 2 5
    3 3 6

Wit the matrix notation, we can also retrieve specific cell entries.

``` r
# specific cell entries
dat[1,1] # cell entry row 1 and column 1
```

    [1] 1

Knowing how to retrieve variables from a data frame, we can now apply
operations on them. The basic principles of vectorization remain the
same. Consider the following set of commands, where we define a
numerical vector `a` and multiply it with the first column from `dat`.

``` r
a <- c(1,2,3)
a * dat[, 1] 
```

    [1] 1 4 9

Importantly, the above code doesn’t change the object `dat` but only
returns the output of the operation. To store the outputs in `dat`, we
use the assignment operator `<-` again and point it to the position in
`dat` where we want to store the outputs. For instance, we can create a
new column `v` and attach it to the existing data frame.

``` r
dat[, "v"] <- a * dat[, 1] 
dat
```

      x y z v
    1 1 4 7 1
    2 2 5 8 4
    3 3 6 9 9

Or we can overwrite an existing column:

``` r
# overriding
dat[, 4] <- dat[, 4] / a
dat
```

      x y z v
    1 1 4 7 1
    2 2 5 8 2
    3 3 6 9 3

#### Functions

Simple arithmetic operations are by no means the only possible
operations in `R`. You already came across other operations in the form
of *functions* such as `c()` `data.frame()` or `rep()`. Moreover,
multiple operations can be combined to realize more complex
computations. However, irrespective of the exact operation, the more
often you need it, the less inclined you should be to write the code
from scratch over and over and over again. Copying and pasting the code
can circumvent the problem of writing from scratch, but it comes with
its very own pitfalls. As a general rule of thumb for programming —
statistical or otherwise — you want to reduce code
duplications/repetitions.

One way to reduce duplications in your `R` code is to use functions.
Functions encapsulate a set of commands that perform a particular
computation and extract only the parts that are changing between runs.
To illustrate, consider you want to run the same set of operations on
multiple variables. Although the only thing that actually changes
between runs is the variable, you might be inclined to send the entire
code to the console over and over again. Encapsulating a code in a
function allows you to reduce many lines to just a *function name* and
some arguments (the changing part), which usually makes up no more than
a line. This results in code and scripts that are much cleaner, easier
to read, and ultimately less prone to errors.

In the following, we briefly consider how functions are build. A
function generally consists of three parts:

1.  Function name
2.  Function inputs: Objects and other arguments such as default
    values/settings, that are used in the code body when indicated.
3.  The code body `{}`: Specifies the operations on the inputs and
    retutns an output.

To illustrate, consider the following code, which defines a function
`summing()`.

``` r
summing <- # function name
  function(x){ # inputs 
    
    # body 
    total <- 0
    for (i in 1:length(x)) {
    total <- total + x[i]
  }
  return(total)
    
}
```

At this point, we don’t need to understand all particularities in the
function body. The main idea is that we have now created a function
`summing()`, which sums up all values within numeric vectors.
Specifically, the function `summing()` takes as input a vector,
substitutes the placeholder `x` in the above code with this vector, and
then executes the code. To see the function in action, we just need to
write the name of the function and put the vector in the input bracket.

``` r
x <- c(1,2,3)
summing(x)
```

    [1] 6

Of course, computing the sum of a vector is a common operation and there
is already a function in place for that:

``` r
sum(x)
```

    [1] 6

In general, because many operations are fairly common and useful to
different data analysis projects, others might have already written a
function for the operation you need. Some of these functions might come
with the initial `R` installation such as `data.frame()`, `rep()`, and
`sum()`, while others are obtained by installing additional *packages*
that are freely available online. Either way, the important take-away is
that before you start to repeatedly write endless lines of code or
racking your brain to come up with a function, you can search online to
see if there is already a function that does what you need.

One last thing about `R` code and functions: You can combine the
operations of multiple functions by placing functions into one another.
The way it works is by reading the `R` code from the inside to the
outside. More specifically, a function returns an output. The output of
the inner (or enclosed) function can then be used as input for the outer
function. To illustrate, consider the following example that combines
the `sum()` function and the `abs()` function, which returns the
non-negative absolute value of scalars. First, we run the operations one
by one by saving the output of `sum()` in an object, which is then used
as input for `abs()`. Thereafter, we do it at once.

``` r
x <- c(-1, -2, -3)
sum_x <- sum(x)
sum_x
```

    [1] -6

``` r
abs(sum_x)
```

    [1] 6

``` r
abs(sum(x))
```

    [1] 6

#### Packages

To wrap up this short tutorial, let’s briefly elaborate a bit more on
*packages*. `R` packages may entail functions, data, and documentation
that are not provided with the standard installation of `R`. `R`
packages are created and maintained by a community of developers and
users who share their work with others. (Maybe you will develop a
helpful package in the future, too.) These packages can be downloaded
from the internet and installed in `R` using the function
`install.packages()`. Once installed, the functions and data in the
package can be made available by loading them with the function
`library()`. (You only have to install packages once, but load them
every time you restart `R`.)

In the following, we install the `tidyverse` package, which itself
contains a collection of packages that offer a rich and powerful set of
functions for working with data frames and analyzing data.

``` r
install.packages("tidyverse")
library("tidyverse")
```

Besides the `tidyverse`, there are thousands of other `R` packages
available for a wide range of tasks, from data manipulation and
visualization to statistical modeling and machine learning. Using
packages will save you a lot of time and effort, allowing you to focus
on what you are here for: data analysis.
