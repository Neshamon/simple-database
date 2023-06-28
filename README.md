


### [Simple Database Project](https://gigamonkeys.com/book/practical-a-simple-database.html)


#### I will be using SBCL Common Lisp in this article




## Essentials of Lisp



```lisp

(defun make-cd (title artist rating ripped)
  "Creates a record"
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db*
  "Creates a db"
  nil)

```



### The Basics


The first function `make-cd` is a function that creates records. It takes these values as arguments:

*  `title`
*  `artist`
*  `rating`
*  `ripped`


After taking these values the function then creates a list of keywords and the given parameters
specified above. But you might ask how lisp knows how or even _when_ to do this. To understand this,
let's take a step back for a moment to understand some fundamentals of Common Lisp.


#### The Basics of the Basics


The fundamental building block of everything in Lisp is the S-Expression. S-Expressions, or sexprs,
are the representation of objects within Common Lisp; Any and everything in Lisp is an object.
These sexprs can be represented in two main ways, as an `atom` or a basic type of list
called a `cons`

Now you may be wondering why there are so many parentheses in Common Lisp. The reason why
is because of the `cons` data structure. Lists in Lisp are delimited by parentheses.
You can verify this by running these functions in a sbcl lisp repl:


```lisp

CL-USER> (consp (list :title 'title :artist 'artist :rating 'rating :ripped 'ripped))
```


```lisp

CL-USER> (listp (list :title 'title :artist 'artist :rating 'rating :ripped 'ripped))
```

The functions `listp` and `consp` check if the given value or sequence is a list/cons 
respectively and returns `T` (Lisp's version of true) or `nil` (Lisp's false) otherwise.

These functions also show something else that's important. Even though we tested the same sequence,
they both returned `T`, meaning that we can know that a `cons` is a `list` and
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

Wow! That's a lot to take in isn't it? What is essentially happening in this function is the `list`
function is turning the sequence of values into a list. 

The second portion of this comparison is the chain of `conses`. Like I said earlier,
a `cons` is a pair of two values. This is well represented in this large function
when you understand how the `cons` function works.

The `cons` function creates a `cons` list by pairing the first argument to the second argument.
This is why you see me chaining all of them together in order to get a long enough list to fit 8 values.
Even at the last sexpr of the `cons` chain, you can see me pair `ripped` 
with an empty set of parentheses. This shows that `cons` will always take two arguments
and not any less.

The third part of this function is at the beginning, where we have our `equal` operator. 
`Equal` takes two arguments and returns `T` if the two objects are equal and `nil` 
otherwise.

If you evaluate this function you will see that it returns `T`, meaning that these two lists
are equal. Isn't that odd? These two lists don't look remotely the same! Not only that,
Why does the value `equal` at the beginning execute, but the value `:title` or 
any other value in these lists not execute?


#### Flow of Execution


If Lisp were to have any type of concrete syntax, it would be this.
Every list in Lisp follows a syntax similar to this:


```lisp

(<operator> <arg1> <arg2> ... <argn>)
```

You can see this pattern in every function call to `cons` and in the call to `list`
as well. Because an operator or function will always be at the beginning of a list, it's one way
Lisp differentiates between what's a function, variable, data structure, method, or macro.

This operator is applied to every argument that comes after it. It's a little difficult to
understand how the `equal` operater applies to each argument, so let's look at an easier example:


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

This is just one fraction of incredible amount flexibility you get when
it comes to writing any program in Lisp. In non-Lisp languages there is 
usually always a stricter syntax that guides the user to program in
a specific way. With Lisp, the flexible syntax imposes very few restrictions on
how you may want to write your program.


#### The make-cd function


So if we go back to the list in our first function, `make-cd`, we can be evaluated as such: 
`(:title . title)`

```lisp
(defun add-record (cd)
  "Adds a record to the db" 
  (push cd *db*))

(add-record (make-cd "My first cd" "Me" 4000000 t))

(defun dump-db ()
  "Pretty prints db info"
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  "Prompt read helper function"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  "Creates prompts for user input. Allowing them to
specify their own records."
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  "Adds multiple records to db with user prompts"
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: "))
            (return))))

(defun save-db (filename)
  "Saves db to a file by printing the db to the given filename"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax 
      (print *db* out))))

(defun load-db (filename)
  "Assigns the stream, input, to db"
  (with-open-file (input filename)
    (with-standard-io-syntax 
      (setf *db* (read input)))))

(defun select (select-fn)
  "Selects a certain record based off of the selector function, select-p"
  (remove-if-not select-fn *db*))

(defun where-fn (&key title artist rating (ripped nil ripped-p))
  "Returns records based off of given parameters if they exist, otherwise returns T"
  #'(lambda (cd)
      (and
       (if title 
           (equal (getf cd :title) title) 
           t)
       (if artist 
           (equal (getf cd :artist) artist) 
           t)
       (if rating 
           (equal (getf cd :ratizng) rating) 
           t)
       (if ripped-p 
           (equal (getf cd :ripped) ripped)
           t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  "Updates db based off of selector-fn by mapping the
 updated row created by the lambda function to the original db"
  (setf *db* 
        (mapcar 
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title
                   (setf (getf row :title) title))
               (if artist 
                   (setf (getf row :artist) artist))
               (if rating 
                   (setf (getf row :rating) rating))
               (if ripped-p
                   (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun make-comparison-expr (field value)
  "Compares the field of cd to the given value"
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  "Compares multiple fields of cd by looping through the fields list 
and utilizing make-comparison-expr to compare every 2 fields
and returns an accumulated list"
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  "Compares all values of the accumulated list returned by make-comparisons-list"
  `#'(lambda (cd) (and ,@(make-comparisons-list clause)))) ; The ,@ syntax splices values together within a list


```
