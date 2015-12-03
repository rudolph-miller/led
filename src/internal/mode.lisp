(in-package :cl-user)
(defpackage led.internal.mode
  (:use :cl)
  (:export :*modes*
           :*current-mode*
           :*mode-changed-hooks*
           :current-mode))
(in-package :led.internal.mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar *modes* (list :normal :insert :command-line))

(defvar *current-mode* :normal)

(defvar *mode-changed-hooks* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current-mode

(defun current-mode ()
  *current-mode*)

(defun (setf current-mode) (mode)
  (assert (member mode *modes*))
  (let ((prev *current-mode*))
    (setq *current-mode* mode)
    (dolist (fn *mode-changed-hooks*)
      (funcall fn mode prev))
    t))

