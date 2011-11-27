;;;; project.lisp

(in-package #:release-wrangler)

(defclass project ()
  ((name
    :initarg :name
    :accessor name)))

(defgeneric current-version (project))
(defgeneric check-out (project))
(defgeneric checkout-directory (project))
(defgeneric name (project)
  (:method ((string string))
    string))
(defgeneric update-system-file (project new-version))
(defgeneric update-documentation (project new-version))
(defgeneric commit (project new-version))
(defgeneric tag (project new-version))
(defgeneric push-upstream (project))
(defgeneric publish (project))
(defgeneric git-url (project))

(defmethod git-url (project)
  (format nil "git://github.com/xach/~A.git"
          (name project)))

(defmethod check-out (project)
  (run "git" "clone" (git-url project)))

(defmethod commit (project new-version)
  (run "git" "add" ".")
  (run "git" "commit" "-m" (format nil "Updated version to ~A."
                                   new-version)))

(defmethod tag (project new-version)
  (run "git" "tag" (format nil "release-~A" new-version)))

(defmethod push-upstream (project)
  (run "git" "push" "origin" "master"))

(defgeneric release (project new-version)
  (:method (project new-version)
    (in-temporary-directory (format nil "~A/" (name project))
      (check-out project)
      (with-posix-cwd (name project)
        (let ((current-version (current-version project)))
          (unless (member new-version
                          (successive-versions current-version)
                          :test 'equalp)
            (error "Bad version transition ~A -> ~A"
                   current-version new-version)))
        (update-system-file project new-version)
        (update-documentation project new-version)
        (commit project new-version)
        (tag project new-version)
        (break "~A" *default-pathname-defaults*)))))
