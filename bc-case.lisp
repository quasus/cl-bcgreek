(in-package #:bcgreek)

#|

We adopt the simplest algorithm imaginable: consecutive characters are examined in a tree of CASEs until a decision can be made whether they represent a polytonic letter and which one, if they do.  The CASE tree is huge and includes thousands of variants.  Yet it can be conveniently generated.

The entry point for the analysis is the BC-CASE.  It takes no arguments.  However, it makes assumptions about the lexical environment where it is expanded.

BC-CASE it accesses characters using the NEXT-CHAR & CURRENT-CHAR forms.  Neither of them takes arguments.  It is assumed that the values of the forms are either characters or NIL.  NEXT-CHAR examines the `next character' and returns NIL if none is available.  CURRENT-CHAR should return the value of the last NEXT-CHAR invocation.

BC-CASE returns the result wrapped into a RESULT form.  The latter can take keyword parameters :TYPE (CHAR or STRING), :VALUE, :REWIND (T or NIL, an instruction to go one character back), :END (no more characters available).

BC-CASE is also influenced by the AUTO-FINAL-SIGMA-P form which indicates if sigma should be automatically converted to the final form.

The forms can be defined e. g. by MACROLET around an invocation of BC-CASE.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun small-accented-family (letter-name)
    (with-bc-constants (+covered-letters+ +bc-accents+)
      (let ((letters (remove-if-not (lambda (x)
                                      (subsetp `(,letter-name "SMALL") (rest x) :test
                                               #'string=))
                                    +covered-letters+)))
        (append (mapcar (lambda (letter)
                          (let ((char (first letter))
                                (parts (rest letter)))
                            (cons char (mapcan
                                         (lambda (part)
                                           (let ((accent (assoc part +bc-accents+
                                                                :test #'string=)))
                                             (if accent
                                                 (list (second accent))
                                                 nil)))
                                         parts))))
                        letters)))))


  (defun accent-case-tree (accented-family)
    (let ((accents (remove-duplicates (reduce #'append (mapcar #'rest accented-family))))
          (default (first (find-if (lambda (x) (null (rest x))) accented-family)))
          (compounds (remove-if (lambda (x)
                                  (null (rest x)))
                                accented-family)))
      (if (null compounds)
          `(result :type char :value ,default)
          `(case (next-char)
             ,@(loop for a in accents
                     collect (list a (accent-case-tree (loop for (char . accents) in compounds
                                                             when (member a accents)
                                                             collect (cons char (remove a accents))))))
             ((nil) (result :type char :value ,default :end t))
             (otherwise (result :type char :value ,default :rewind t))))))

  (with-bc-constants (+bc-capitals+)
    (defun capital-accent-case-tree (bag &optional (former-candidates +bc-capitals+))
      (let* ((candidates (remove-if-not (lambda (x)
                                          (subsetp bag (rest x)))
                                        former-candidates))
             (exact-match (find-if (lambda (x)
                                     (subsetp (rest x) bag))
                                   candidates)))
        (flet ((res-string (rewind)
                 `(result :type string :value ,(coerce (cons #\* (reverse bag)) 'string) :rewind ,rewind)))
          (cond ((null candidates) (res-string t))
                (exact-match `(result :type char :value ,(first exact-match)))
                (t (let* ((next-parts (set-difference (remove-duplicates (reduce #'append (mapcar #'rest candidates)))
                                                      bag)))
                     `(case (next-char)
                        ,@(loop for part in next-parts
                                collect (list part (capital-accent-case-tree (cons part bag) candidates)))
                        ((nil) ,(res-string nil))
                        (otherwise ,(res-string t))))))))))

  (defun sigma-clause ()
    (with-bc-constants (+medial-sigma+ +final-sigma+ +lunate-sigma+ +bc-letters+)
      `(case (next-char)
         (#\1 (result :type char :value ,+medial-sigma+))
         (#\2 (result :type char :value ,+final-sigma+))
         (#\3 (result :type char :value ,+lunate-sigma+))
         (otherwise (if (not (auto-final-sigma-p))
                        (result :type char :value ,+medial-sigma+ :rewind t)
                        (case (current-char)
                          ((#\' ,@(loop for c in (mapcar #'first +bc-letters+)
                                        nconc (list c (char-upcase c))))
                           (result :type char :value ,+medial-sigma+ :rewind t))
                          ((nil) (result :type char :value ,+final-sigma+ :end t))
                          (otherwise (result :type char :value ,+final-sigma+ :rewind t)))))))))

(defmacro bc-case ()
  (with-bc-constants (+bc-letters+ +bc-misc+ +final-sigma+)
    `(case (next-char)
       ,@(loop for (bc name) in +bc-letters+
               unless (string= name "SIGMA")
               collect (list (list bc (char-upcase bc)) (accent-case-tree (small-accented-family name))))
       ((#\s #\S) ,(sigma-clause))
       (#\j (result :type char :value ,+final-sigma+))
       (#\* ,(capital-accent-case-tree '()))
       ,@(loop for (bc code) in +bc-misc+
               collect (list bc `(result :type char :value ,(code-char code))))
       ((nil) (result :end t))
       (otherwise (result :type char :value (current-char))))))
