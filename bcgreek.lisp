(in-package #:bcgreek)

(defun bcgreek (string &optional auto-final-sigma-p)
  (check-type string string)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0) (safety 0))))
  (with-output-to-string (output-stream)
    (let ((pos -1)
          (length (length string)))
      (declare (fixnum pos))
      (macrolet ((next-char ()
                   '(if (< (the fixnum (incf pos)) length)
                        (char string (the fixnum pos))
                        nil))
                 (current-char ()
                   '(char string (the fixnum pos)))
                 (result (&key type value rewind end)
                   `(progn
                      ,(when rewind
                         '(the fixnum (decf pos)))
                      ,(case type
                         (char `(write-char ,value output-stream))
                         (string `(write-string ,value output-stream)))
                      ,(when end
                         '(return))))
                 (auto-final-sigma-p ()
                   'auto-final-sigma-p))
        (loop
          (bc-case))))))
