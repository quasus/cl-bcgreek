(in-package #:bcgreek)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun split-by-spaces (string)
    (let ((p (position #\Space string)))
      (if p
          (cons (subseq string 0 p) (split-by-spaces (subseq string (1+ p))))
          (list string))))

  (defun covered-letters (greek-letters bc-letters bc-accent-chars)
    "Among all the greek-letters take only those whose names consist of letter names as in bc-letters, accent names as in bc-accents & the words \"GREEK\", \"SMALL\", &c.  Collect the characters and their names split by spaces: ((#\GREEK_CAPITAL_LETTER_ALPHA \"GREEK\" \"CAPITAL\" \"LETTER\" \"ALPHA\") ...)."
    (let ((all-parts (append (list "GREEK" "SMALL" "CAPITAL" "LETTER" "WITH" "AND")
                             (mapcar #'second bc-letters)
                             (mapcar #'first bc-accent-chars))))
      (loop for (code name) in greek-letters
            for parts = (split-by-spaces name)
            when (subsetp parts all-parts :test #'string=)
            collect (cons (code-char code) parts))))

  (defun bc-capitals (covered-letters bc-letters bc-accents)
    (mapcar (lambda (x)
              (cons (first x)
                    (loop for part in (set-difference (rest x)
                                                      '("GREEK" "CAPITAL" "LETTER" "WITH" "AND")
                                                      :test #'string=)
                          collect (second (assoc part (append bc-accents
                                                              (mapcar #'reverse bc-letters))
                                                 :test #'string=)))))
            (remove-if (lambda (x)
                         (member "SMALL" (rest x) :test #'string=))
                       covered-letters)))) 

(defmacro define-bc-constants (&key greek-letters bc-letters bc-accents bc-misc)
  (let* ((+covered-letters+ (covered-letters greek-letters bc-letters bc-accents))
         (+bc-capitals+ (bc-capitals +covered-letters+ bc-letters bc-accents)))
    `(defmacro with-bc-constants (constants &body body)
       (let ((all-constants `((+bc-letters+ ',',bc-letters)
                              (+bc-accents+ ',',bc-accents)
                              (+bc-misc+ ',',bc-misc)
                              (+covered-letters+ ',',+covered-letters+)
                              (+bc-capitals+ ',',+bc-capitals+)
                              (+medial-sigma+ #.(code-char #x03C3))
                              (+final-sigma+ #.(code-char #x03C2))
                              (+lunate-sigma+ #.(code-char #x03F2)))))
         `(let ,(loop for constant in constants
                 when (assoc constant all-constants)
                 collect it)
            ,@body)))))
