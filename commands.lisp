;;;; commands.lisp

(in-package #:release-wrangler)

(defun scp (file &key user host remote-path)
  (run "scp" (truename file)
       (format nil "~@[~A@~]~A:~@[~A~]"
               user host remote-path)))

(defun ssh (user host command &key identity-file)
  (when (eql user t)
    (setf user nil))
  (run "ssh"
       (when identity-file
         (list "-i" (truename identity-file)))
       (format nil "~@[~A@~]~A" user host)
       command))

(defun sign (file &key as output)
  (unless output
    (setf output (parse-namestring (concatenate 'string (namestring file)
                                                ".asc"))))
  (restart-case
      (when (probe-file output)
        (error "Output file ~S already exists" output))
    (delete (&optional v)
      :report "Delete output file and proceed"
      (declare (ignore v))
      (delete-file output)))
  (run gpg
       (when as
         (list :local-user as))
       :output output
       :no-tty :sign :detach "-a" (truename file)))
