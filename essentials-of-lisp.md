<a id="x-28SIMPLE-DB-3A-40ESSENTIALS-OF-LISP-20MGL-PAX-3ASECTION-29"></a>
<a id="SIMPLE-DB:@ESSENTIALS-OF-LISP%20MGL-PAX:SECTION"></a>

# Essentials of Lisp

## Table of Contents

- [1 The Basics][3e78]
- [2 The Basics of the Basics][77a6]
- [3 Flow of Execution][0fd9]
- [4 Symbols][b896]

###### \[in package SIMPLE-DB\]
<a id="x-28SIMPLE-DB-3A-40THE-BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="SIMPLE-DB:@THE-BASICS%20MGL-PAX:SECTION"></a>

## 1 The Basics



The first function is a function that creates records.
It takes these values as arguments:

- title

- artist

- rating

- ripped

After taking these values the function then creates a list of keywords and the given parameters
specified above. But you might ask how Lisp knows how or even *when* to do this. To understand this,
let's take a step back to look at some fundamentals of Common Lisp



<a id="x-28SIMPLE-DB-3A-40THE-BASICS-OF-THE-BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="SIMPLE-DB:@THE-BASICS-OF-THE-BASICS%20MGL-PAX:SECTION"></a>

## 2 The Basics of the Basics



The fundamental building block of everything in Lisp is the *S-Expression*. *S-Expressions*, or sexprs,
are the representation of objects within Common Lisp; Any and everything in Lisp is an object.
These sexprs can be represented in two main ways, as an `atom`([`0`][5152] [`1`][a657]) or a basic type of *list*
called a `cons`([`0`][a237] [`1`][12a8])

Now you may be wondering why there are so many parentheses in Common Lisp. The reason why
is because of the `cons` data structure. Lists in Lisp are delimited by parentheses.
You can verify this by running these functions in a sbcl lisp repl:

```lisp
CL-USER> (consp (list :title 'title :artist 'artist :rating 'rating :ripped 'ripped))
```

```lisp
CL-USER> (listp (list :title 'title :artist 'artist :rating 'rating :ripped 'ripped))
```

The functions [`listp`][fefd] and [`consp`][6d19] check if the given value or sequence is a list/cons 
respectively and returns `T` (Lisp's version of true) or `nil` (Lisp's false) otherwise.

These functions also show something else that's important. Even though we tested the same sequence,
they both returned `T`, meaning that we can know that a `cons` is a `list`([`0`][79d8] [`1`][6d9f]) and
a `list` is a `cons`

Knowing this, it's quite easy to see how everything in Lisp is made up of lists. But where exactly
does the `cons` list fit in among all these lists? The `cons` list is the most fundamental
list, it consists of a pair of two values and can be denoted as: `( value1 . value2 )`

And nearly everything in Common Lisp is represented by these `cons` lists.
If you run this function in the lisp repl, you can see how a cons list and a regular list are equal:

```lisp
CL-USER> (equal (list :title 'title 
                      :artist 'artist 
                      :rating 'rating 
                      :ripped 'ripped) 
                (cons :title 
                      (cons 'title 
                            (cons :artist 
                                  (cons 'artist 
                                        (cons :rating 
                                              (cons 'rating 
                                                    (cons :ripped 
                                                          (cons 'ripped '())))))))))
```

Wow! That's a lot to take in isn't it? What is essentially happening in this function is the [`list`][6d9f]
function is turning the sequence of values into a list. 

The second portion of this comparison is the chain of *conses*. Like I said earlier,
a `cons` is a pair of two values. This is well represented in this large function
when you understand how the [`cons`][12a8] function works.

The [`cons`][12a8] function creates a `cons` list by pairing the first argument to the second argument.
This is why you see me chaining all of them together in order to get a long enough list to fit 8 values.
Even at the last sexpr of the `cons` chain, you can see me pair `'ripped` 
with an empty set of parentheses. This shows that `cons` will always take two arguments
and not any less.

The third part of this function is at the beginning, where we have our [`equal`][3fb5] operator. 
`Equal` takes two arguments and returns `T` if the two objects are equal and `nil`
otherwise.

If you evaluate this function you will see that it returns `T`, meaning that these two lists
are equal. Isn't that odd? These two lists don't look remotely the same! Not only that,
Why does the value `equal` at the beginning execute, but the value `:title` or 
any other value in these lists not execute?



<a id="x-28SIMPLE-DB-3A-40FLOW-OF-EXECUTION-20MGL-PAX-3ASECTION-29"></a>
<a id="SIMPLE-DB:@FLOW-OF-EXECUTION%20MGL-PAX:SECTION"></a>

## 3 Flow of Execution



If Lisp were to have any type of concrete syntax, it would be this.
Every list in Lisp follows a syntax similar to this:

```lisp
(<operator> <arg1> <arg2> ... <argn>)
```

You can see this pattern in every function call to `cons`([`0`][a237] [`1`][12a8]) and in the call to `list`([`0`][79d8] [`1`][6d9f])
as well. Because an operator or function will usually always be at the beginning of a list.

This operator is applied to every argument that comes after it. It's a little difficult to
understand how the [`equal`][3fb5] operater applies to each argument, so let's look at an easier example:

```lisp
(+ 1 2)
```

This is simple addition between two integers in Lisp. In the same way we would add 1 to 2 to get
a sum of three, so would we apply the operator of `equal` to each argument 
in the previous functions. Another way to look at it would be this way:

```lisp
(+ 1 2 3 4 5 6 7 8 9)                   ; => 45
(+ (+ 1 2) (+ 3 4) (+ 5 6) (+ 7 8) 9)   ; => 45
(+ 3 7 11 15 9)                         ; => 45
(+ (+ 3 7) (+ 11 15) 9)                 ; => 45
(+ 10 26 9)                             ; => 45
(+ (+ 10 26) 9)                         ; => 45
(+ 36 9)                                ; => 45
45 ; This is an atom
```

When you look at the code above, you can see how the addition operator is applied to its arguments
in pairs. For every pair of arguments we are performing the operation of addition.
In the same way in which we perform addtion on the arguments of the addition 
operator, so would we any other operator. So the equal operator would look something
like this:

```lisp
(equal (equal 1 (equal 1 (equal 2 "2"))) (equal 1 (equal 1 (equal 2 "2"))))  ; => T
(equal (equal 1 (equal 1 nil)) (equal 1 (equal 1 nil)))  ; => T
(equal (equal 1 nil) (equal 1 nil))  ; => T
(equal nil nil)  ; => T
T
```

Now after looking a little closer at these two functions, wouldn't you say that this looks
a little familiar? If we refer back to our `cons` list, we would see a similar
pattern of function calls whether it's the addition operator or the equals operator.
For addition it would look like:

```lisp
(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 9))))))))
;; Or
(equal (equal 1 (equal 1 (equal 2 "2"))) (equal 1 (equal 1 (equal 2 "2"))))
```

If you look closely at these functions and how we structured them,
you'll notice we presented the same result in multiple ways;
Every iteration of the addition sexprs were all different ways to say the same thing.
But what if I told you this phenomena was going on in more ways than one?



<a id="x-28SIMPLE-DB-3A-40SYMBOLS-20MGL-PAX-3ASECTION-29"></a>
<a id="SIMPLE-DB:@SYMBOLS%20MGL-PAX:SECTION"></a>

## 4 Symbols



Let's go back to our variable `*db*` and let me show you something interesting:

```lisp
(defparameter *db* 33)

(defun *db* (*db*)
(+ *db* *db*))

(*db* *db*)

(function-lambda-expression (symbol-function '*db*))
```

After looking at this code, do you think something like this would work?
Or is the lisp repl just going to throw errors at us concerning names?
Well if you guessed that it would work, then you'd be correct

What's going on is that a global variable called `*db*` is created with the value of 33.
After that a function also with the name of `*db*`. . . Wait what? How can a function
share the name of `*db*` with a variable simultaneously? This is because of a data type
called a symbol

As we discussed before, everything in Common Lisp is made up of @bold{Sexprs}, which represent objects.
We also know that these Sexprs can either be categorized as atoms or cons cells/lists/singly linked
lists. But there is one more fundamental representation we ought to talk about, and it's a [`symbol`][e5af]

In Algol-based languages (Languages like C), the primitive or composite data type is typically a
string literal

This is just one fraction of incredible amount flexibility you get when
it comes to writing any program in Lisp. In non-Lisp languages there is 
usually always a stricter syntax that guides the user to program in
a specific way. With Lisp, the flexible syntax imposes very few restrictions on
how you may want to write your program.



  [0fd9]: #SIMPLE-DB:@FLOW-OF-EXECUTION%20MGL-PAX:SECTION "Flow of Execution"
  [12a8]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cons.htm "CONS (MGL-PAX:CLHS FUNCTION)"
  [3e78]: #SIMPLE-DB:@THE-BASICS%20MGL-PAX:SECTION "The Basics"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [5152]: http://www.lispworks.com/documentation/HyperSpec/Body/f_atom.htm "ATOM (MGL-PAX:CLHS FUNCTION)"
  [6d19]: http://www.lispworks.com/documentation/HyperSpec/Body/f_consp.htm "CONSP (MGL-PAX:CLHS FUNCTION)"
  [6d9f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_list_.htm "LIST (MGL-PAX:CLHS FUNCTION)"
  [77a6]: #SIMPLE-DB:@THE-BASICS-OF-THE-BASICS%20MGL-PAX:SECTION "The Basics of the Basics"
  [79d8]: http://www.lispworks.com/documentation/HyperSpec/Body/t_list.htm "LIST (MGL-PAX:CLHS CLASS)"
  [a237]: http://www.lispworks.com/documentation/HyperSpec/Body/t_cons.htm "CONS (MGL-PAX:CLHS CLASS)"
  [a657]: http://www.lispworks.com/documentation/HyperSpec/Body/t_atom.htm "ATOM (MGL-PAX:CLHS TYPE)"
  [b896]: #SIMPLE-DB:@SYMBOLS%20MGL-PAX:SECTION "Symbols"
  [e5af]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL (MGL-PAX:CLHS CLASS)"
  [fefd]: http://www.lispworks.com/documentation/HyperSpec/Body/f_listp.htm "LISTP (MGL-PAX:CLHS FUNCTION)"
