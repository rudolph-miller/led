(in-package :cl-user)
(defpackage led.buffer
  (:use :cl)
  (:import-from :led.util
                :make-vector-with)
  (:import-from :led.character
                :character-to-ichar)
  (:import-from :led.line
                :*max-line-width*
                :make-line
                :line-eol-p
                :line-length
                :migrate-line-to-line
                :line-chars-with-padding)
  (:import-from :led.string
                :string-to-lines
                :lines-to-string
                :insert-new-line-to-lines
                :insert-ichar-to-lines)
  (:import-from :led.window
                :*window*
                :window-width
                :window-height
                :window-x
                :window-y
                :window-lines)
  (:export :buffer
           :buffer-name
           :buffer-lines
           :insert-new-line-at-point
           :insert-ichar-at-point
           :insert-new-line
           :insert-ichar
           :migrate-buffer
           :prev-line
           :next-line
           :cursor-up
           :cursor-down
           :cursor-left
           :cursor-right))
(in-package :led.buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defparameter *current-buffer* nil)

(defparameter *buffers* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer

(defclass buffer ()
  ((name :accessor buffer-name
         :initarg :name
         :initform "No name buffer")
   (position-x :accessor buffer-position-x
               :initarg :position-x
               :initform 0)
   (position-y :accessor buffer-position-y
               :initarg :position-y
               :initform 0)
   (width :accessor buffer-width
          :initarg :width
          :initform (window-width *window*))
   (height :accessor buffer-height
           :initarg :height
           :initform (window-height *window*))
   (x :accessor buffer-x
      :initform 0)
   (y :accessor buffer-y
      :initform 0)
   (top-row :accessor buffer-top-row
            :initform 0)
   (lines :accessor buffer-lines
          :initarg :lines)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize hooks

(defmethod initialize-instance :before ((buffer buffer) &rest initargs)
  (declare (ignore initargs))
  (assert *window*))

(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
  (declare (ignore initargs))
  (push-buffer buffer)
  (setq *current-buffer* buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *buffers* operations

(defun push-buffer (buffer)
  (push buffer *buffers*))

(defun delete-buffer (buffer)
  (setq *buffers* (remove buffer *buffers*)))

(defun pop-buffer (buffer)
  (delete-buffer buffer)
  buffer)

(defun buffer-visible-lines (buffer)
  (let ((top-row (buffer-top-row buffer)))
    (subseq (buffer-lines buffer)
            top-row
            (min (length (buffer-lines buffer))
                 (+ top-row (buffer-height buffer))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *max-line-width*

(defmacro with-buffer-max-line-width (buffer &body body)
  `(let ((*max-line-width* (1- (buffer-width ,buffer))))
     (progn ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-content

(defun set-buffer-content (string &optional (buffer *current-buffer*))
  (with-buffer-max-line-width buffer
    (setf (buffer-lines buffer)
          (string-to-lines string))))

(defun buffer-content (&optional (buffer *current-buffer*))
  (lines-to-string (buffer-lines buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert

(defun insert-new-line-at-point (y &optional (buffer *current-buffer*))
  (setf (buffer-lines buffer)
        (insert-new-line-to-lines y (buffer-lines buffer))))

(defun insert-ichar-at-point (ichar x y &optional (buffer *current-buffer*))
  (with-buffer-max-line-width buffer
    (setf (buffer-lines buffer)
          (insert-ichar-to-lines ichar x y (buffer-lines buffer)))))

(defun insert-new-line (&optional (buffer *current-buffer*))
  (let ((y (+ (buffer-top-row buffer) (buffer-y buffer))))
    (insert-new-line-at-point y buffer)))

(defun insert-ichar (ichar &optional (buffer *current-buffer*))
  (let ((x (buffer-x buffer))
        (y (+ (buffer-top-row buffer) (buffer-y buffer))))
    (insert-ichar-at-point ichar x y buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; migarte

;; FIXME: Support multi buffers (like split window)
;; (defun migrate-buffers ())

(defun migrate-buffer-line (line window y start end)
  (loop with win-lines = (window-lines window)
        for ichar across (line-chars-with-padding line (- end start))
        for x from start
        do (setf (aref win-lines y x) ichar)
        finally (unless (line-eol-p line)
                  (setf (aref win-lines y x) (character-to-ichar #\\)))))

(defun buffer-name-line (buffer)
  (with-buffer-max-line-width buffer
    (let* ((name (buffer-name buffer))
           (lines (string-to-lines name)))
      (aref lines 0))))

(defun migrate-buffer (&optional (buffer *current-buffer*) (window *window*))
  (let ((x (+ (buffer-position-x buffer) (buffer-x buffer)))
        (y (+ (buffer-position-y buffer) (buffer-y buffer)))
        (lines (buffer-visible-lines buffer)))
    (setf (window-x window) x)
    (setf (window-y window) y)
    (loop for line across lines
          for y from 0 below (1- (buffer-height buffer))
          for win-row from (buffer-position-y buffer)
          with win-col-start = (buffer-position-x buffer)
          with win-col-end = (+ win-col-start (buffer-width buffer))
          do (migrate-buffer-line line window win-row win-col-start win-col-end)
          finally (migrate-buffer-line (buffer-name-line buffer)
                                       window
                                       (1+ win-row)
                                       win-col-start
                                       win-col-end))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer controllers

(defun prev-line (&optional (buffer *current-buffer*))
  (when (> (buffer-top-row buffer) 0)
    (decf (buffer-top-row buffer))))

(defun next-line (&optional (buffer *current-buffer*))
  (when (< (buffer-top-row buffer)
           (- (length (buffer-lines buffer))
              (buffer-height buffer)))
    (incf (buffer-top-row buffer))))

(defun cursor-up (&optional (buffer *current-buffer*))
  (if (> (buffer-y buffer) 0)
      (decf (buffer-y buffer))
      (prev-line buffer)))

(defun cursor-down (&optional (buffer *current-buffer*))
  (if (< (buffer-y buffer)
         (1- (min (length (buffer-lines buffer))
                  (buffer-height buffer))))
      (incf (buffer-y buffer))
      (next-line buffer)))

(defun cursor-left (&optional (buffer *current-buffer*))
  (let* ((lines (buffer-lines buffer))
         (y (+ (buffer-top-row buffer)
               (buffer-y buffer)))
         (prev-line (and (> y 0)
                         (aref lines (1- y)))))
    (cond
      ((> (buffer-x buffer) 0)
       (decf (buffer-x buffer))
       t)
      ((and prev-line
            (not (line-eol-p prev-line)))
       (setf (buffer-x buffer) (1- (line-length prev-line)))
       (decf (buffer-y buffer))
       t)
      (t nil))))

(defun cursor-right (&optional (buffer *current-buffer*))
  (let* ((lines (buffer-lines buffer))
         (y (+ (buffer-top-row buffer)
               (buffer-y buffer)))
         (current-line (aref lines y)))
    (cond
      ((< (buffer-x buffer)
          (1- (line-length current-line)))
       (incf (buffer-x buffer))
       t)
      ((not (line-eol-p current-line))
       (setf (buffer-x buffer) 0)
       (incf (buffer-y buffer))
       t)
      (t nil))))
