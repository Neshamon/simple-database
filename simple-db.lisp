
(defun make-cd (title artist rating ripped)
  "Creates a record"
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db*
  "Creates a db"
  nil)

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
   (prompt-read "Ripped [y/n]")))

