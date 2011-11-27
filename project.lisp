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
(defgeneric publish (project new-version))
(defgeneric git-url (project))
(defgeneric git-tag-string (project new-version))

(defmethod git-url (project)
  (format nil "git@github.com:xach/~A.git"
          (name project)))

(defmethod check-out (project)
  (run "git" "clone" (git-url project)))

(defmethod commit (project new-version)
  (run "git" "add" ".")
  (run "git" "commit" "-m" (format nil "Updated version to ~A."
                                   new-version)))

(defmethod git-tag-string (project new-version)
  (format nil "release-~A" new-version))

(defmethod tag (project new-version)
  (run "git" "tag" (git-tag-string project new-version)))

(defmethod push-upstream (project)
  (run "git" "push" "origin" "master")
  (run "git" "push" "--tags" "origin" "master"))

(defmethod suffixed-pathname (pathname suffix)
  (values
   (parse-namestring (concatenate 'string (namestring pathname)
                                  "."
                                  suffix))))

(defmethod publish (project new-version)
  (let* ((base (merge-pathnames "../"))
         (html (merge-pathnames "../lisp/"))
         (name (name project))
         (tar (make-pathname :name name :type "tar"
                             :defaults base))
         (gz (suffixed-pathname tar "gz"))
         (tgz (make-pathname :name name :type "tgz"
                             :defaults base))
         (asc (suffixed-pathname tgz "asc")))
    (run "git" "archive"
         :format "tar"
         :prefix (format nil "~A-~A/" name new-version)
         :output tar
         (git-tag-string project new-version))
    (run "gzip" "-v" tar)
    (rename-file gz tgz)
    (sign tgz)
    (alexandria:copy-file tgz (merge-pathnames html tgz))
    (alexandria:copy-file asc (merge-pathnames html asc))
    (run "cp" "-a" "doc/" (concatenate 'string "../lisp/" name))
    (with-posix-cwd "../"
      (run "tar" "czvf" "release-wrangler.tgz" "lisp/")
      (scp "release-wrangler.tgz"
           :host "bradley.xach.com"
           :remote-path "tmp/"))
    (ssh t "bradley.xach.com"
         "cd www.xach.com && tar xzvf ~/tmp/release-wrangler.tgz")))


(defgeneric release (project new-version)
  (:method (project new-version)
    (in-temporary-directory (format nil "~A/" (name project))
      (let ((bundle-base (merge-pathnames "lisp/")))
        (ensure-directories-exist bundle-base)
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
          (push-upstream project)
          (publish project new-version))))))
