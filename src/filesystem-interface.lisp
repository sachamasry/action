(in-package :cl-user)
(defpackage :action/filesystem-interface
  (:use :cl)
  (:import-from :uiop
   :ensure-pathname
                :probe-file*
   :delete-file-if-exists
                :truename*
   :directory-exists-p
                :file-exists-p
   :ensure-pathname
                :ensure-directory-pathname
   :resolve-location
                :resolve-absolute-location
   :pathname-directory-pathname
                :absolute-pathname-p
   :relative-pathname-p
                :file-pathname-p
   :split-name-type)
  (:import-from :alexandria
   :when-let)
  (:import-from :cl-strings
   :join
                :split
   :clean
                :camel-case
   :kebab-case
                :snake-case)
  (:export #:absolute-directory-from-path
           #:construct-directory
           #:split-path
           #:construct-destination-directory
           #:construct-file-name
           #:construct-destination-path
           #:get-system-source-directory ))
(in-package :action/filesystem-interface)

#|
FILE MANAGEMENT

This section builds utilities to help with file management, parsing and generating
path names.
|#
(defun absolute-directory-from-path (path &key exists-p)
  "Return directory PATH points to."
  (when (and path
             (or (pathnamep path)
                 (and (stringp path)
                      (not (zerop (length path))))))
    (if exists-p
        (probe-file*
         (nth-value 0 (split-path path)))
        (nth-value 0 (split-path path)))))

(defun construct-directory (absolute-directory relative-directory)
  "Construct and return a valid new directory by adding a RELATIVE-DIRECTORY
component to an ABSOLUTE-DIRECTORY."
  (make-pathname
   :directory (append
               (pathname-directory absolute-directory)
               (list relative-directory))))

(defun split-path (path)
  "Return PATH split into its constituent components as return values:
- directory
- full file name
- file name
- file type.

Adapted from Weitz, E. (Common Lisp Recipes, 2016, p.446)"
  (when (and path
             (or (pathnamep path)
                 (and (stringp path)
                      (not (zerop (length path))))))
    (let ((file-name (file-namestring path)))
      (if (and file-name (plusp (length file-name)))
          (multiple-value-bind (name type)
              (split-name-type file-name)
            (values (directory-namestring path)
                    file-name name type))
          (values (directory-namestring path) file-name NIL NIL)))))

(defun construct-destination-directory
    (&key source-path destination-directory ensure-directory-exists-p) 
  "Return a fully constructed and valid destination directory, given SOURCE-PATH
and DESTINATION-DIRECTORY components.

Scenarios:

DESTINATION-DIRECTORY is provided, and it is an absolute path, then construct
the destination directory else return an error. This is the default desired
behaviour.

Return: 'DESTINATION-DIRECTORY'

SOURCE-PATH is provided and is (or it's parent directory) is an absolute path,
construct a destination path from it, else return an error.

Return: 'parent directory of SOURCE-PATH'

If SOURCE-PATH is provided with an additional, optional, DESTINATION-DIRECTORY,
and DESTINATION-DIRECTORY is a relative path, then construct a destination path
including this.

Return: 'parent directory of SOURCE-PATH/DESTINATION-DIRECTORY'

Optionally, check that the constructed destination directory exists."
  (cond ((and destination-directory
              (absolute-pathname-p destination-directory))
         (if ensure-directory-exists-p
             (absolute-directory-from-path destination-directory :exists-p t)
             (absolute-directory-from-path destination-directory)))
        ((and source-path
              (absolute-pathname-p source-path)
              (if ensure-directory-exists-p
                  (absolute-directory-from-path source-path :exists-p t)
                  t))
         (if destination-directory
             (and
              (relative-pathname-p destination-directory)
              (when-let ((constructed-directory
                          (construct-directory
                           (absolute-directory-from-path source-path)
                           destination-directory)))
                (if ensure-directory-exists-p
                    (absolute-directory-from-path constructed-directory :exists-p t)
                    (absolute-directory-from-path constructed-directory))))
             (if ensure-directory-exists-p
                 (absolute-directory-from-path source-path :exists-p t)
                 (absolute-directory-from-path source-path))))))

(defun construct-destination-path
    (&key source-path destination-directory destination-file-name
       destination-file-prefix destination-file-suffix
       (destination-file-extension NIL extension-provided-p) (separator "-")
       use-source-file-name)
  "Return a fully constructed and valid destination PATH, given
components.

Scenarios:

DESTINATION-DIRECTORY, -FILE-NAME and -FILE-EXTENSION are provided,
and DESTINATION-DIRECTORY is an absolute path, then construct a destination
path concatenating them, else return an error. This is the default desired
behaviour.

Return: 'DESTINATION-DIRECTORY/DESTINATION-FILE-NAME.DESTINATION-FILE-EXTENSION'

SOURCE-PATH, DESTINATION-FILE-NAME and -FILE-EXTENSION are provided,
and SOURCE-PATH (or it's parent directory) is an absolute path, construct
a destination path by concatenating them, else return an error.

Return: 'parent directory of SOURCE-PATH/DESTINATION-FILE-NAME.DESTINATION-FILE-EXTENSION'

If DESTINATION-DIRECTORY is additionally supplied, and it is a relative path, then
construct a destination path including this.

Return: 'parent directory of SOURCE-PATH/DESTINATION-DIRECTORY/
         DESTINATION-FILE-NAME.DESTINATION-FILE-EXTENSION'

In all cases, the DESTINATION-FILE-NAME and DESTINATION-FILE-EXTENSION are
expected.

Optionally, a file name prefix and/or suffix may be provided to the destination
name, separated by the SEPARATOR string. "
  (when
      (and (or destination-file-name (file-pathname-p source-path))
           (or destination-file-extension extension-provided-p))
    (make-pathname :directory (pathname-directory
                               (probe-file
                                (ensure-directories-exist 
                                 (construct-destination-directory
                                  :source-path source-path
                                  :destination-directory destination-directory))))
                   :name (construct-file-name 
                          () "-"
                          destination-file-prefix
                          (or 
                           destination-file-name
                           (nth-value 2
                                      (split-path source-path)))
                          destination-file-suffix)
                   :type destination-file-extension)))

(defun construct-file-name (file-extension separator
                            &rest name-components)
  "Construct file name given NAME-COMPONENTS, separated by SEPARATOR string,
returning combined name as a full name, including a FILE-EXTENSION string, if
it's a non-empty string.

FILE-EXTENSION is mandatory, but may be empty ('') or NIL. If it is provided, it
must be a string. No checking is done for illegal characters on your file
system, or if it's a valid file extension.

SEPARATOR is mandatory, but may be empty ('') or NIL. If it is provided, it must
be a string, and will be used to delineate individual NAME-COMPONENTS within the
destination file name.

NAME-COMPONENTS is not mandatory, but the function will immediately return NIL
if no components are provided. Components may be any number of: numbers, strings
or NIL; NIL values and zero-length strings will be pruned prior to the
construction of the file name.

CONSTRUCT-FILE-NAME returns a string with the combined file name, or NIL if any
conditions aren't met."
  (flet ((file-name (separator &rest components)
           (join
            (remove-if #'(lambda (component)
                           (or (null component)
                               (and (stringp component)
                                    (zerop (length component)))))
                       name-components)
            :separator separator)))
    (when name-components
      (if (and file-extension
               (not (zerop (length file-extension))))
          (nth-value 1
                     (split-path
                      (make-pathname
                       :name (file-name separator name-components)
                       :type file-extension)))
          (file-name separator name-components)))))


(defun get-system-source-directory (system)
  ""
  (asdf:system-source-directory
   (asdf/find-system:find-system system)))
