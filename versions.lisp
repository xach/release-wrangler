;;;; versions.lisp

(in-package #:release-wrangler)

(defun successive-versions (version)
  "Return reasonable candidates for successive versions of VERSION."
  (let ((parts (mapcar 'parse-integer (split-sequence #\. version))))
    (labels ((incremented (v)
               (let ((v+ (copy-list v)))
                 (incf (first (last v+)))
                 v+))
             (extended (v)
               (append v (list 1)))
             (rejoin (v)
               (format nil "~{~D~^.~}" v)))
      (mapcar #'rejoin
              (list* (extended parts)
                     (mapcar #'incremented
                             (maplist 'reverse (reverse parts))))))))

(defun after-search (substring string)
  (let ((pos (search substring string)))
    (when pos
      (+ pos (length substring)))))

(defun system-line-version (line)
  (let ((pos (after-search ":version " line)))
    (when pos
      (values (read-from-string line t t :start pos)
              pos))))

(defun system-file-version (file)
  (block nil
    (flet ((maybe-return-version (line)
             (let ((version (system-line-version line)))
               (when version
                 (return version)))))
      (with-open-file (stream file)
        (loop for line = (read-line stream nil)
              while line do (maybe-return-version line)))
      (error "No :version keyword seen in ~A" file))))

(defmethod current-version (project)
  (system-file-version (make-pathname :name (name project)
                                      :type "asd")))

(defun filter-file (fun file)
  "Filter each line of FILE with FUN. FUN should return NIL if the
  input line in FILE should not be filtered, or a replacement
  line (without newline) otherwise. Returns T if any line was
  filtered"
  (setf file (truename file))
  (let ((temp-file (make-pathname :type "tmp-filter"
                                  :defaults file))
        (filtered nil))
    (unwind-protect
         (progn
           (with-open-file (out temp-file
                                :direction :output
                                :if-exists :error)
             (with-open-file (in file)
               (loop for line = (read-line in nil)
                     while line do
                     (let ((filter (funcall fun line)))
                       (cond (filter
                              (setf filtered t)
                              (write-line filter out))
                             (t
                              (write-line line out)))))))
           (rename-file temp-file file))
      (ignore-errors (delete-file temp-file)))
    filtered))

(defmethod update-system-file (project new-version)
  (flet ((filter (line)
           (multiple-value-bind (version indent)
                  (system-line-version line)
             (when version
               (format nil "~v@T:version ~S" indent new-version)))))
    (let ((file (make-pathname :name (name project)
                               :type "asd")))
      (unless (filter-file #'filter file)
        (error "No version found in ~A" file)))))

(defmethod update-documentation (project new-version)
  (flet ((filter (line)
           (let ((pos (search "The latest version is " line)))
             (when pos
               (unless (zerop pos)
                 (error "Unexpected positioning of version in docs ~
                         -- offset ~D"
                        pos))
               (format nil "The latest version is ~A, released on ~A."
                       new-version (pretty-date-string))))))
    (let ((file #p"doc/index.html"))
      (unless (filter-file #'filter file)
        (error "No version found in ~A" file)))))
