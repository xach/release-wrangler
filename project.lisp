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
(defgeneric git-tag-string (project new-version))
(defgeneric tags (project))
(defgeneric latest-tag (project)
  (:method (project)
    (first (last (tags project)))))

(defmethod checkout-directory (project)
  (make-pathname :directory (list :absolute
                                  :home "release-wrangler" (name project))))

(defmethod git-url (project)
  (format nil "git@github.com:xach/~A.git"
          (name project)))

(defmethod check-out (project)
  (ensure-directories-exist "~/release-wrangler/")
  (with-posix-cwd "~/release-wrangler/"
    (if (probe-file (checkout-directory project))
        (with-posix-cwd (checkout-directory project)
          (run "git" "pull"))
        (run "git" "clone" (git-url project)))))

(defmethod commit (project new-version)
  (run "git" "add" ".")
  (run "git" "commit" "-m" (format nil "Updated version to ~A."
                                   new-version)))

(defmethod git-tag-string (project new-version)
  (format nil "release-~A" new-version))

(defmethod tag (project new-version)
  (in-project-directory project
    (run "git" "tag" (git-tag-string project new-version))))

(defmethod push-upstream (project)
  (in-project-directory project
    (run "git" "push" "origin" "master")
    (run "git" "push" "--tags" "origin" "master")))

(defmethod suffixed-pathname (pathname suffix)
  (values
   (parse-namestring (concatenate 'string (namestring pathname)
                                  "."
                                  suffix))))

(defmacro in-project-directory (project &body body)
  (let ((proj (copy-symbol 'project))
        (path (copy-symbol 'path)))
    `(let* ((,proj ,project)
            (,path (checkout-directory ,proj)))
       (unless (probe-file ,path)
         (check-out ,proj))
       (with-posix-cwd (checkout-directory ,proj)
         ,@body))))

(defmethod tags (project)
  (in-project-directory project
    (run-output-lines "git" "tag" "-l")))




(defmethod publish (project)
  (with-temporary-directory (base "~/release-wrangler/publish/")
    (let* ((html (merge-pathnames "lisp/" base))
           (name (name project))
           (tar (make-pathname :name name :type "tar"
                               :defaults base))
           (gz (suffixed-pathname tar "gz"))
           (tgz (make-pathname :name name :type "tgz"
                               :defaults base))
           (asc (suffixed-pathname tgz "asc"))
           (version (current-version project)))
      (ensure-directories-exist html)
      (in-project-directory project
        (run "git" "archive"
             :format "tar"
             :prefix (format nil "~A-~A/" name version)
             :output tar
             (git-tag-string project version))
        (run "gzip" "-v" tar)
        (rename-file gz tgz)
        (sign tgz)
        (alexandria:copy-file tgz (merge-pathnames html tgz))
        (alexandria:copy-file asc (merge-pathnames html asc))
        (run "cp" "-a" "doc/" (merge-pathnames name html)))
      (with-posix-cwd base
        (run "tar" "czvf" "release-wrangler.tgz" "lisp/")
        (scp "release-wrangler.tgz"
             :host "bradley.xach.com"
             :remote-path "tmp/"))
      (ssh t "bradley.xach.com"
           "cd www.xach.com && tar xzvf ~/tmp/release-wrangler.tgz"))))


(defgeneric release (project new-version)
  (:method (project new-version)
    (let ((path (checkout-directory project))
          (name (name project)))
      (check-out project)
      (with-posix-cwd path
        (let ((current-version (current-version project)))
          (unless (member new-version
                          (successive-versions current-version)
                          :test 'equalp)
            (error "Bad version transition ~A -> ~A"
                   current-version new-version)))
        (update-system-file project new-version)
        (update-documentation project new-version)
        (commit project new-version)
        (tag project new-version)))))
