(in-package :cl-user)
(defpackage led.buffer.buffer
  (:use :cl)
  (:import-from :led.internal.character
                :character-to-ichar)
  (:import-from :led.internal.line
                :make-line
                :line-ichars
                :line-eol-p
                :line-length
                :line-ichars-with-padding
                :string-to-line)
  (:import-from :led.internal.string
                :string-to-lines
                :lines-to-string)
  (:import-from :led.window
                :*window*
                :window-width
                :window-height
                :window-x
                :window-y
                :window-lines
                :redraw)
  (:export :buffer
           :buffer-name
           :buffer-lines
           :redraw-buffer
           :prev-line
           :next-line
           :cursor-up
           :cursor-down
           :cursor-left
           :cursor-right
           :insert-new-line-at-point
           :insert-ichar-at-point
           :insert-new-line
           :insert-ichar))
(in-package :led.buffer.buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar *current-buffer* nil)

(defvar *buffers* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer

(defclass buffer ()
  ((position-x :accessor buffer-position-x
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
          :initarg :lines)
   (visible-lines :accessor buffer-visible-lines)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fold

(defun fold (sequence buffer)
  (loop with max-line-width = (1- (buffer-width buffer))
        with pos = 0
        with seq-len = (length sequence)
        for fold-p = (and max-line-width
                          (> (- seq-len pos)
                             max-line-width))
        for content = (if fold-p
                          (let ((end (+ pos max-line-width)))
                            (prog1 (subseq sequence pos end)
                              (setq pos end)))
                          (subseq sequence pos))
        collecting content
        while fold-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-window-cursor-position

(defun buffer-window-cursor-position (buffer)
  (assert (buffer-visible-lines buffer))
  (let ((x (buffer-x buffer))
        (y (- (buffer-y buffer) (buffer-top-row buffer))))
    (loop for line across (buffer-visible-lines buffer)
          for index from 0
          while (< index y)
          do (loop repeat (length (fold (line-ichars line) buffer))
                   for i = index
                     then (progn (when (> y i) (incf y))
                                 (incf index))))
    (loop with max-line-width = (1- (buffer-width buffer))
          while (>= x max-line-width)
          do (decf x max-line-width)
             (incf y))
    (values x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-name

(defgeneric buffer-name (buffer)
  (:method ((buffer buffer))
    "No Name Buffer"))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-content

(defun set-buffer-content (string &optional (buffer *current-buffer*))
  (setf (buffer-lines buffer)
        (if string
            (string-to-lines string)
            #())))

(defun buffer-content (&optional (buffer *current-buffer*))
  (lines-to-string (buffer-lines buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; migarte

(defun buffer-migrate-lines (buffer length)
  (loop with top-row = (buffer-top-row buffer)
        with result = (make-array length :initial-element (make-line))
        with lines = (buffer-lines buffer)
        for y from top-row below (length (buffer-lines buffer))
        with result-length = (length result)
        for index from 0 below result-length
        for line = (aref lines y)
        collecting line into visible-lines
        do (loop for ichars in (fold (line-ichars line) buffer)
                 for i = index
                   then (incf index)
                 while (< i result-length)
                 do (setf (aref result i) (make-line :ichars ichars))
                 finally (when (< i result-length)
                           (setf (line-eol-p (aref result i)) t)))
        finally (setf (buffer-visible-lines buffer) (apply #'vector visible-lines))
                (return result)))

;; FIXME: Support multi buffers (like split window)
;; (defun migrate-buffers ())

(defun migrate-buffer-line (line window y start end)
  (loop with win-lines = (window-lines window)
        for ichar across (line-ichars-with-padding line (- end start))
        for x from start
        do (setf (aref win-lines y x) ichar)
        finally (unless (line-eol-p line)
                  (setf (aref win-lines y x) (character-to-ichar #\\)))))

(defun buffer-name-line (buffer)
  (let* ((name (buffer-name buffer))
         (line (when name (string-to-line (car (fold name buffer))))))
    (when line
      (setf (line-eol-p line) t)
      line)))

(defun migrate-buffer (&optional (buffer *current-buffer*) (window *window*))
  (let* ((name-line (buffer-name-line buffer))
         (migrate-line-length (if name-line
                                  (1- (buffer-height buffer))
                                  (buffer-height buffer)))
         (lines (buffer-migrate-lines buffer migrate-line-length)))
    (multiple-value-bind (x y) (buffer-window-cursor-position buffer)
      (loop for line across lines
            for win-row from (buffer-position-y buffer)
            with win-col-start = (buffer-position-x buffer)
            with win-col-end = (+ win-col-start (buffer-width buffer))
            do (migrate-buffer-line line window win-row win-col-start win-col-end)
            finally (when name-line
                      (migrate-buffer-line name-line
                                           window
                                           (1- (buffer-height buffer))
                                           win-col-start win-col-end)))
      (setf (window-x window) x)
      (setf (window-y window) y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redraw-buffer

(defun redraw-buffer (&optional (buffer *current-buffer*))
  (migrate-buffer buffer)
  (redraw)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer controllers

(defun normalize-x (buffer)
  (let ((current-line-length (line-length
                              (aref (buffer-lines buffer)
                                    (buffer-y buffer)))))
    (if (zerop current-line-length)
        (setf (buffer-x buffer) 0)
        (setf (buffer-x buffer)
              (min (buffer-x buffer) (1- current-line-length))))))

(defun prev-line (&optional (buffer *current-buffer*))
  (when (> (buffer-top-row buffer) 0)
    (let ((name-line (buffer-name-line buffer)))
      (when (= (buffer-y buffer)
               (+ (buffer-top-row buffer)
                  (- (buffer-height buffer)
                     (if name-line 3 2))))
        (decf (buffer-y buffer))))
    (decf (buffer-top-row buffer))
    (normalize-x buffer)
    (redraw-buffer buffer)))

(defun next-line (&optional (buffer *current-buffer*))
  (when (< (buffer-top-row buffer)
           (- (length (buffer-lines buffer))
              (buffer-height buffer)))
    (when (= (buffer-y buffer)
             (buffer-top-row buffer))
      (incf (buffer-y buffer)))
    (incf (buffer-top-row buffer))
    (normalize-x buffer)
    (redraw-buffer buffer)))

(defun cursor-up (&optional (buffer *current-buffer*))
  (multiple-value-bind (x y) (buffer-window-cursor-position buffer)
    (declare (ignore x))
    (prog1
        (cond
          ((> y 0)
           (decf (buffer-y buffer)) t)
          ((prev-line)
           (decf (buffer-y buffer)) t)
          (t nil))
      (normalize-x buffer)
      (redraw-buffer buffer))))

(defun cursor-down (&optional (buffer *current-buffer*))
  (multiple-value-bind (x y) (buffer-window-cursor-position buffer)
    (declare (ignore x))
    (let ((name-line (buffer-name-line buffer)))
      (prog1
          (cond
            ((< y (- (buffer-height buffer) (if name-line 2 1)))
             (incf (buffer-y buffer)) t)
            ((next-line buffer)
             (incf (buffer-y buffer)) t)
            (t nil))
        (normalize-x buffer)
        (redraw-buffer buffer)))))

(defun cursor-left (&optional (buffer *current-buffer*))
  (when (> (buffer-x buffer) 0)
    (decf (buffer-x buffer))
    (redraw-buffer buffer)))

(defun cursor-right (&optional (buffer *current-buffer*))
  (let* ((lines (buffer-lines buffer))
         (current-line (aref lines (buffer-y buffer))))
    (when (< (buffer-x buffer) (1- (line-length current-line)))
      (incf (buffer-x buffer))
      (redraw-buffer buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert

(defun insert-new-line-at-point (y &optional (buffer *current-buffer*))
  (let ((lines (buffer-lines buffer)))
    (setf (buffer-lines buffer)
          (concatenate 'vector
                       (subseq lines 0 y)
                       (vector (make-line :eol-p t))
                       (subseq lines y))))
  (redraw-buffer)
  t)

(defun insert-ichar-at-point (ichar x y &optional (buffer *current-buffer*))
  (let* ((line (aref (buffer-lines buffer) y))
         (ichars (line-ichars line)))
    (setf (line-ichars line)
          (concatenate 'vector
                       (subseq ichars 0 x)
                       (vector ichar)
                       (subseq ichars x))))
  (redraw-buffer)
  t)

(defun insert-new-line (&optional (buffer *current-buffer*))
  (insert-new-line-at-point (buffer-y buffer) buffer))

(defun insert-ichar (ichar &optional (buffer *current-buffer*))
  (insert-ichar-at-point ichar (buffer-x buffer) (buffer-y buffer) buffer))

