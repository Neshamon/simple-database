


## [Link here](https://gigamonkeys.com/book/practical-a-simple-database.html)




# Essential Functions



```lisp

(defun make-cd (title artist rating ripped)
  "Creates a record"
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db*
  "Creates a db"
  nil)

```




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
