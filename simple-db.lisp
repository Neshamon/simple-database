#|

@title Implementation of Practical Lisp's Simple Database Project
@subsubsection @link{https://gigamonkeys.com/book/practical-a-simple-database.html}{Link here}
@subsubsubsection I will be using SBCL Common Lisp in this article

@author John Matthews
@syntax erudite

@subsection Essential Functions

@ignore
|#

(ql:quickload :erudite)

#|
@end ignore

|#

(defun make-cd (title artist rating ripped)
  "Creates a record"
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db*
  "Creates a db"
  nil)

#|

@subsubsection The Basics

The first function @bold{make-cd} is a function that creates records. It takes these values as arguments:
@list
@item @verb{title}
@item @verb{artist}
@item @verb{rating}
@item @verb{ripped}
@end list

After taking these values the function then creates a list of keywords and the given parameters
specified above. But you might ask how lisp knows how or even @it{when} to do this. To understand this,
let's take a step back for a moment to understand some fundamentals of Common Lisp.

@subsubsubsection The Basics of the Basics

The fundamental building block of everything in Lisp is the S-Expression. S-Expressions, or sexprs,
are the representation of objects within Common Lisp, and anything and everything in Lisp is an object.
These sexprs can be represented in two main ways, as an @bold{atom} or a basic type of list
called a @bold{cons}

Now you may be wondering why there are so many parentheses in Common Lisp. The reason why
is because of the @bold{cons} data structure. Lists in Lisp are delimited by parentheses.
You can verify this by running this function in a sbcl lisp repl:

@code
CL-USER> (consp (list :title 'title :artist 'artist :rating 'rating :ripped 'ripped))
@end code

The function @bold{consp} checks if the given value is a cons and returns @bold{T} (Lisp's version of true)
or @bold{nil} (Lisp's false) otherwise.

Knowing this, it is quite easy to see how everything in Lisp is made up of lists. But where exactly
does the @bold{cons} list fit in among all these lists? The @bold{cons} list is the most fundamental
list, it consists of a pair of two values and can be denoted as: 

@code
( value1 . value2 )
@end code

And nearly everything in common lisp is represented by these @bold{cons} lists.
If you run this function in the lisp repl, you can see how a cons list and a regular list are equal:

@code
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
@end code

Wow! That's a lot to take in isn't it? What is essentially happening in this function is the @bold{list}
function is turning the sequence of values into a list. 

The second portion of this comparison is the chain of @bold{conses}. Like I said @ref{earlier},
a @bold{cons} is a pair of two values

Another name for these lists are cons cells.

So if we go back to the list in our first function, @bold{make-cd}, we can be evaluated as such:

@code
((:title . title) .
@end code
|#
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


;; @ignore

;; Org export
(erudite:erudite #p"~/quicklisp/local-projects/simple-database/test.org" 
                 #p"~/quicklisp/local-projects/simple-database/simple-db.lisp"
                 :output-type :org)

;; Markdown export
(erudite:erudite #p"~/quicklisp/local-projects/simple-database/test.md" 
                 #p"~/quicklisp/local-projects/simple-database/simple-db.lisp"
                 :output-type :markdown)

(ql:quickload :erudite)
;;@end ignore
