(in-package :cl-user)
(defpackage led.buffer.command-line-buffer
  (:use :cl
        :led.internal
        :led.window
        :led.buffer.buffer)
  (:export :*command-line-buffer*
           :*command-line-buffer-height*
           :command-line-buffer
           :on-command-line))
(in-package :led.buffer.command-line-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar *command-line-buffer* nil)

(defvar *command-line-buffer-height* 1)

(defvar *previous-buffer* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command-line-buffer

(defclass command-line-buffer (buffer) ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize-instance

(defun initialize-command-line-buffer (buffer)
  (setf (buffer-height buffer) *command-line-buffer-height*)
  (set-buffer-content nil buffer)
  (setf (buffer-position-x buffer) 0)
  (setf (buffer-position-y buffer) (1- (window-height *window*))))

(defmethod initialize-instance :after ((buffer command-line-buffer) &rest initargs)
  (declare (ignore initargs))
  (initialize-command-line-buffer buffer))
  
(defmethod initialize-instance :around ((buffer command-line-buffer) &rest initargs)
  (declare (ignore buffer initargs))
  (let ((current *current-buffer*))
    (call-next-method)
    (setq *command-line-buffer* buffer)
    (setq *current-buffer* current)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-status

(defmethod buffer-status ((buffer command-line-buffer))
  (declare (ignore buffer))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-command-line-buffer-content

;; FIXME: doesn't support color

(defun on-command-line (content)
  (assert *command-line-buffer*)
  (set-buffer-content content *command-line-buffer*)
  (redraw-buffer *command-line-buffer*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; focus

(defun focus-on-command-line ()
  (setq *previous-buffer* *current-buffer*)
  (setq *current-buffer* *command-line-buffer*)
  (redraw-buffer)
  t)

(defun unfocus-on-command-line ()
  (setq *current-buffer* *previous-buffer*)
  (setq *previous-buffer* nil)
  (redraw-buffer)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *mode-changed-hooks*

(defun mode-on-command-line (mode prev)
  (let ((string (ecase mode
                  (:normal "")
                  (:insert "-- INSERT --")
                  (:command-line ":"))))
    (on-command-line string)
    (setf (buffer-x *command-line-buffer*) 0)
    (when (eq mode :command-line)
      (setf (buffer-x *command-line-buffer*) 1)
      (focus-on-command-line))
    (when (and (eq prev :command-line)
               (not (eq mode :command-line)))
      (unfocus-on-command-line))))

(unless (member 'mode-on-command-line *mode-changed-hooks*)
  (push 'mode-on-command-line *mode-changed-hooks*))
