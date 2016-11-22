;;; jdee-test.el -- Integrated Development Environment for Java.

;; Author: Matthew O. Smith <matt@m0smith.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004, 2005, 2008 Paul Kinnucan.
;; Copyright (C) 2009 by Paul Landes
;; Copyright (C) 2006-2007 by Suraj Acharya

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is one of a set of packages that make up the Java Development
;; Environment (JDE) for Emacs. See the JDE User's Guide for more
;; information. It includes code for using the Eclipse compiler
;; originally written by Suraj Acharya.

;; This provides access to unit testing support

;; The latest version of the JDE is available at
;; <URL:http://jdee.sourceforge.net/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Landes <landes <at> mailc dt net>

;;; Code:

(defgroup jdee-test-options nil
  "JDE Unit Testing Options"
  :group 'jdee
  :prefix "jdee-test-option-")

(defcustom jdee-test-function 'jdee-junit-run
    "The function to run to do a unit test"
  :group 'jdee-compile-options
  :type 'function)


(defun jdee-test-next-error-function (n &optional reset)
  "This function is a value of `next-error-function' that supports
the results of mvn test. 

Return the tag of the method if found, nil otherwise."
  
  (let ((next-fn 'compilation-next-error-function)
        (compile-buffer (current-buffer)))
    (funcall next-fn n reset)
    (let* ((method-name   (with-current-buffer compile-buffer
                            (get-text-property (point) 'method-name)))
           (class-name   (with-current-buffer compile-buffer
                           (get-text-property (point) 'class-name)))
           (tags (semantic-something-to-tag-table (current-buffer)))
           (class-tag (jdee-maven-find-tag-by-name-and-type class-name 'type tags))
           (class-members (plist-get (nth 2 class-tag) :members))
           (method-tag (jdee-maven-find-tag-by-name-and-type method-name 'function class-members)))
      (when method-tag
        (semantic-go-to-tag method-tag)
        (semantic-momentary-highlight-tag method-tag)
        method-tag))))
      

(defvar jdee-test-error-regexp
  
  (format "%s(%s):\\(.*\\)" (jdee-parse-java-name-part-re) (jdee-parse-java-fqn-re))
  " Looks for something like 

  testConnectionProxy2CallJava2(jde.juci.ConnectionImplTest): expected:<hello worl[ ]d> but was:<hello worl[]d>

  Match regions are
1 - the test method name
2 - FQN of the unit test
3 - package name of the unit test
4 - class name of the unit test
5 - the error message
")


  
(defvar jdee-test-finish-hook nil)


;;
;; Class for running unit tests
;;

(defclass jdee-unit-test-runner (jdee-compile-compiler)
  ()
  "Class of unit test runners.")

(defmethod initialize-instance ((this jdee-unit-test-runner) &rest fields)
  "Initialize by finding the compiler and then updating slots to
run unit test instead of compiling.  Support both running on disk
or using the bsh server."
  ;; Call parent initializer.
  (call-next-method)
  (let* ((compiler  (jdee-compile-get-the-compiler))
	 (name (eieio-object-name-string compiler)))
    (eieio-object-set-name-string this (format "%s %s" name "unit test runner"))
    (oset this path (jdee-get-jdk-prog 'java))
    (oset this exec-method "jde.unittest.UnitTestServer.unitTest")
         
    ;; TODO: There has to be a function to copy slots from one object
    ;; to another
    (with-slots (version buffer window interactive-args use-server-p) compiler
      (when (slot-boundp compiler :version)
	(oset this :version version))
      (when (slot-boundp compiler :buffer)
	(oset this :buffer buffer))
      (when (slot-boundp compiler :window)
	(oset this :window window))
      (when (slot-boundp compiler :interactive-args)
	(oset this :interactive-args interactive-args))
      (when (slot-boundp compiler :use-server-p)
	(oset this :use-server-p use-server-p)))))

(defmethod jdee-compile-command-line-args ((this jdee-unit-test-runner))
  "Add the fully qualified name of the class to the command line."
  (list (jdee-fqn)))

(defmethod jdee-compile-classpath-arg ((this jdee-unit-test-runner))
  "Returns the classpath argument for this unit test runner based
on either' `jdee-global-classpath' or
`jdee-run-option-classpath', perferring the latter."
  (let ((classpath
	 (if jdee-run-option-classpath
	     jdee-run-option-classpath
	   (jdee-get-global-classpath)))
	(symbol
	 (if jdee-run-option-classpath
	     'jdee-run-option-classpath
	   'jdee-global-classpath)))
    (if classpath
	(list
	 "-classpath"
	 (jdee-build-classpath
	  classpath
	  symbol)
	 ))))

(defmethod jdee-compile-run-server ((this jdee-unit-test-runner))
  "Don't show the classpath in the output buffer."
  (let ((jdee-compile-option-hide-classpath t)
        (compile-buffer(oref (oref this buffer) buffer)))
    (with-current-buffer compile-buffer
      (setq next-error-function 'jdee-test-next-error-function)
      (setq compilation-finish-functions
            (lambda (buf msg)
              (run-hook-with-args 'jdee-test-finish-hook buf msg)
              (setq compilation-finish-functions nil)))
      
      (add-to-list 'compilation-error-regexp-alist
                   (list jdee-test-error-regexp
                         'jdee-maven-file nil nil nil
                         2              ;Hyperlink = FQN
                         '(1  compilation-info-face) ;test method name
                         '(2  compilation-error-face) ;FQN
                         '(5  compilation-message-face)))) ; error message

    (call-next-method)))
    
  

;;
;; end of class
;;

(defun jdee-test-function-default ()
  "Default unit test function. "
  (let* ((runner (make-instance 'jdee-unit-test-runner)))

    (jdee-compile-compile runner)))

;;;###autoload
(defun jdee-test-unittest ()
  "Perform unit test.  Delegates to the function specified by `jdee-test-function'."
  (interactive)
  (funcall jdee-test-function))

(provide 'jdee-test)

;;; jdee-test.el ends here
