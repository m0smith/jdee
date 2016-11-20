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

(defcustom jdee-test-function 'jdee-test-function-default
    "Check if we have the new (21.3+) compile.el.
Set this to t if you are running an Emacs with the new compile.el
and want to get slightly better font-locking in the compile
buffer.  A value of nil will force the use of older style
compilation-error-regexp.  This variable tries to auto-detect the
compile.el version by checking if
`compilation-error-regexp-alist-alist' is defined."
  :group 'jdee-compile-options
  :type 'function)

;;
;; Class for running unit tests
;;

(defclass jdee-unit-test-runner (jdee-compile-compiler)
  ()
  "Class of unit test runners.")

(defmethod initialize-instance ((this jdee-unit-test-runner) &rest fields)
  "Initialize by find the compiler for the JDK using it to initialize this instance."
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
  "Get additional command line arguments for this compiler."
  (list (jdee-fqn)))

(defmethod jdee-compile-classpath-arg ((this jdee-unit-test-runner))
  "Returns the classpath argument for this unit test runner."
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
