(in-package :cl-user)
(defpackage led.buffer.command-line-buffer
  (:use :cl
        :led.internal
        :led.window
        :led.buffer.buffer)
  (:export :*command-line-buffer*
           :*command-line-buffer-height*
           :*raw-command-input*
           :*exec-command-package*
           :command-line-buffer
           :stop-command-line-mode
           :exec-command
           :on-command-line))
(in-package :led.buffer.command-line-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar *command-line-buffer* nil)

(defvar *command-line-buffer-height* 1)

(defvar *raw-command-input* nil)

(defvar *exec-command-package* :led)

(defvar *exec-command-and-args* nil)


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
;; start-command-line-mode

(defun start-command-line-mode ()
  (unwind-protect
       (let ((*current-buffer* *command-line-buffer*))
         (setf (buffer-x *command-line-buffer*) 1)
         (setq *exec-command-and-args* nil)
         (on-command-line ":")
         (input-loop))
    (if *exec-command-and-args*
        (let ((symbol (find-symbol (car *exec-command-and-args*)
                                   *exec-command-package*)))
          (if (and symbol (symbol-function symbol))
              (if (cdr *exec-command-and-args*)
                  (apply symbol (cdr *exec-command-and-args*))
                  (funcall symbol))
              (on-command-line "Unknown Command")))
        (on-command-line "Quit"))
    (setq *exec-command-and-args* nil)
    (setf (current-mode) :normal)
    (redraw-buffer)))

(defun detect-start-command-line-mode (next prev)
  (declare (ignore prev))
  (when (eq next :command-line)
    (start-command-line-mode)))

(unless (member 'detect-start-command-line-mode *mode-changed-hooks*)
  (push 'detect-start-command-line-mode *mode-changed-hooks*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stop-command-line-mode

(defun stop-command-line-mode ()
  (assert (eq (current-mode) :command-line))
  (setq *stop-input-loop* t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exec-command

(defun exec-command (&optional args)
  (let ((command (string-trim (list #\NewLine)
                              (subseq (buffer-content *command-line-buffer*) 1))))
    (when (not *raw-command-input*)
      (setq command (string-upcase command)))
    (setq *exec-command-and-args*
          (cons command args))
    (stop-command-line-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *mode-changed-hooks*

(defun mode-on-command-line (mode prev)
  (let ((string (cond
                  ((eq mode :insert) "-- INSERT --")
                  ((and (eq mode :normal) (eq prev :insert)) ""))))
    (when string (on-command-line string))))

(unless (member 'mode-on-command-line *mode-changed-hooks*)
  (push 'mode-on-command-line *mode-changed-hooks*))
