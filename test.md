\documentclass[11pt,pdflatex,makeidx]{scrbook}   % Book class in 11 points
\usepackage[margin=0.5in]{geometry}
\usepackage{color}
\usepackage{makeidx}
\usepackage{hyperref}

\usepackage{listings}

\usepackage{hyperref}
\usepackage{courier}

\hypersetup{colorlinks=true,linkcolor=blue}

\lstloadlanguages{Lisp}
\lstset{frame=none,language=Lisp,
  basicstyle=\ttfamily\small,
  keywordstyle=\color{black}\bfseries,
  stringstyle=\ttfamily,
  showstringspaces=false,breaklines}
\lstnewenvironment{code}{}{}

\parindent0pt  \parskip10pt             % make block paragraphs
\raggedright                            % do not right justify
% Note that book class by default is formatted to be printed back-to-back.
\makeindex
\begin{document}                        % End of preamble, start of text.
\title{\bf Implementaion of Practical Lisp's Simple Database Project}

\subtitle{link{https://gigamonkeys.com/book/practical-a-simple-database.html}{Link here}}


\author{John Matthews}

\date{\today}                           %   Use current date.
\frontmatter                            % only in book class (roman page #s)
\maketitle                              % Print title page.
\tableofcontents                        % Print table of contents
\mainmatter                             % only in book class (arabic page #s)
\long\def\ignore#1{}




\section{Essential Functions}

\index{*db*}
\label{*db*}
\index{make-cd}
\label{make-cd}

\begin{code}

(defun make-cd (title artist rating ripped)
  "Creates a record"
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db*
  "Creates a db"
  nil)

\end{code}



\index{where}
\label{where}
\index{make-comparisons-list}
\label{make-comparisons-list}
\index{make-comparison-expr}
\label{make-comparison-expr}
\index{update}
\label{update}
\index{where-fn}
\label{where-fn}
\index{select}
\label{select}
\index{load-db}
\label{load-db}
\index{save-db}
\label{save-db}
\index{add-cds}
\label{add-cds}
\index{prompt-for-cd}
\label{prompt-for-cd}
\index{prompt-read}
\label{prompt-read}
\index{dump-db}
\label{dump-db}
\index{add-record}
\label{add-record}

\begin{code}
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



(erudite:erudite #p"~/quicklisp/local-projects/simple-database/test.md" 
                 (file-namestring 
                  #p"/home/neshamon/quicklisp/local-projects/simple-database/simple-db.lisp"))
\end{code}
                             % ignore macro
\chapter{Index}
\printindex
\end{document}