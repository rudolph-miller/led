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
                :append-ichar-to-line
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
           :migrate-buffer))
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
  (declare (ignore buffer initargs))
  (assert *window*))
                                        
(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
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
;; buffer-content

(defun set-buffer-content (buffer string)
  (let ((*max-line-width* (1- (buffer-width buffer))))
    (setf (buffer-lines buffer)
          (string-to-lines string))))

(defun get-buffer-content (buffer)
  (lines-to-string (buffer-lines buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert

(defun insert-new-line-at-point (y &optional (buffer *current-buffer*))
  (setf (buffer-lines buffer)
        (insert-new-line-to-lines y (buffer-lines buffer))))

(defun insert-ichar-at-point (ichar x y &optional (buffer *current-buffer*))
  (setf (buffer-lines buffer)
        (insert-ichar-to-lines ichar x y (buffer-lines buffer))))

(defun insert-new-line (&optional (buffer *current-buffer*))
  (insert-new-line-at-point (buffer-y buffer) buffer))

(defun insert-ichar (ichar &optional (buffer *current-buffer*))
  (insert-ichar-at-point ichar
                         (buffer-x buffer)
                         (buffer-y buffer)
                         buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; migarte

;; FIXME: Support multi buffers (like split window)
;; (defun migrate-buffers ())

(defun migrate-buffer-line (line window y start end)
  (loop with win-lines = (window-lines window)
        for ichar across (line-chars-with-padding line (- end start))
        for x from start
        do (setf (aref win-lines y x) ichar)))

(defun migrate-buffer (&optional (buffer *current-buffer*) (window *window*))
  (let ((x (+ (buffer-position-x buffer) (buffer-x buffer)))
        (y (+ (buffer-position-y buffer) (buffer-y buffer)))
        (lines (buffer-visible-lines buffer)))
    (setf (window-x window) x)
    (setf (window-y window) y)
    (loop for line across lines
          for y from 0 below (buffer-height buffer)
          for win-row from (buffer-position-y buffer)
          with win-col-start = (buffer-position-x buffer)
          with win-col-end = (+ win-col-start (buffer-width buffer))
          do (migrate-buffer-line line window win-row win-col-start win-col-end))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer controllers

(defun move-buffer-lines-up (&optional (buffer *current-buffer*))
  (when (> (buffer-top-row buffer) 0)
    (decf (buffer-top-row buffer))))

(defun move-buffer-lines-down (&optional (buffer *current-buffer*))
  (when (< (buffer-top-row buffer)
           (- (length (buffer-lines buffer))
              (buffer-height buffer)))
    (incf (buffer-top-row buffer))))

(defun move-buffer-cursor-up (&optional (buffer *current-buffer*))
  (when (> (buffer-y buffer) 0)
    (decf (buffer-y buffer))))

(defun move-buffer-cursor-down (&optional (buffer *current-buffer*))
  (when (< (buffer-y buffer)
           (1- (min (length (buffer-lines buffer))
                    (buffer-height buffer))))
    (incf (buffer-y buffer))))

(defun move-buffer-cursor-right (&optional (buffer *current-buffer*))
  (let* ((y (buffer-y buffer))
         (lines (buffer-lines buffer))
         (current-line (aref lines y)))
    (when (< (buffer-x buffer)
             (1- (line-length current-line)))
      (incf (buffer-x buffer)))))

(defun move-buffer-cursor-left (&optional (buffer *current-buffer*))
  (when (> (buffer-x buffer) 0)
    (decf (buffer-x buffer))))

(defun move-buffer-cursor-or-lines-down (&optional (buffer *current-buffer*))
  (unless (move-buffer-cursor-down buffer)
    (move-buffer-lines-down buffer)))

(defun move-buffer-cursor-or-lines-up (&optional (buffer *current-buffer*))
  (unless (move-buffer-cursor-up buffer)
    (move-buffer-lines-up buffer)))
