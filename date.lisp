;;;; date.lisp

(in-package #:release-wrangler)

(defvar *pretty-months*
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defparameter *nths*
  #("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th"))

(defun ordinal-suffix (i)
  (cond ((<= 10 i 20) "th")
        (t
         (aref *nths* (mod i 10)))))

(defun ordinal (number)
  (format nil "~D~A" number (ordinal-suffix number)))

(defun pretty-date-string (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format nil "~A ~A, ~A"
            (aref *pretty-months* (1- month))
            (ordinal date)
            year)))
