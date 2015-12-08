(in-package :cl-user)
(defpackage led.buffer.buffer
  (:use :cl
        :led.internal
        :led.window)
  (:export :*current-buffer*
           :*buffers*
           :buffer
           :buffer-position-x
           :buffer-position-y
           :buffer-width
           :buffer-height
           :buffer-cursor
           :buffer-top-row
           :buffer-status
           :buffer-top-line
           :buffer-content
           :set-buffer-content
           :redraw-buffer
           :prev-line
           :next-line
           :cursor-up
           :cursor-down
           :cursor-left
           :cursor-right
           :cursor-left-most
           :cursor-right-most
           :delete-line
           :delete-ichar
           :delete-prev-ichar
           :replace-ichar
           :insert-new-line
           :insert-next-line
           :insert-eol
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
           :initform (1- (window-height *window*)))
   (cursor :accessor buffer-cursor)
   (lines :accessor buffer-top-line
          :initarg :lines)
   (visible-top-line :accessor buffer-visible-top-line)
   (visible-bottom-line :accessor buffer-visible-bottom-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current-line

(defun buffer-current-line (buffer)
  (when (buffer-cursor buffer)
    (bdl-ichar-line (buffer-cursor buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-status

(defgeneric buffer-status (buffer)
  (:method ((buffer buffer))
    "No Name Buffer"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize-instance

(defmethod initialize-instance :before ((buffer buffer) &rest initargs)
  (declare (ignore initargs))
  (assert *window*))

(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
  (declare (ignore initargs))
  (push-buffer buffer)
  (setq *current-buffer* buffer))

(defmethod initialize-instance :around ((buffer buffer) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (when (slot-boundp buffer 'lines)
    (redraw-buffer)))


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
;; initialize-cursor

(defun initialize-cursor (buffer)
  (if (zerop (line-length (buffer-top-line buffer)))
      (setf (buffer-cursor buffer) nil)
      (setf (buffer-cursor buffer)
            (next (line-top-bdl-ichar
                   (next (buffer-top-line buffer))))
            (buffer-visible-top-line buffer)
            (next (buffer-top-line buffer))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-content

(defun buffer-content (&optional (buffer *current-buffer*))
  (lines-to-string (buffer-top-line buffer)))

(defun set-buffer-content (string &optional (buffer *current-buffer*))
  (prog1 (setf (buffer-top-line buffer)
               (string-to-lines string))
    (initialize-cursor buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; migarte

(defun fold-ichars (ichars buffer)
  (loop with max-line-width = (1- (buffer-width buffer))
        with current-length = 0
        for ichar across ichars
        for ichar-width = (ichar-width ichar)
        with start = 0
        for position from 0
        when (> (+ current-length ichar-width) max-line-width)
          collecting (prog1 (subseq ichars start position)
                       (setq current-length 0)
                       (setq start position))
            into result
        do (incf current-length ichar-width)
        finally (return
                  (append result (list (subseq ichars start))))))

(defun buffer-board (buffer length)
  (loop with result = (make-array length :initial-element #())
        with eol-lines = nil
        for index from 0 below length
        for prev-line = nil then line
        for line = (buffer-visible-top-line buffer) then (next line)
        while line
        do (loop for ichars
                   in (fold-ichars (bdl-ichars-to-ichars
                                    (line-top-bdl-ichar line))
                                   buffer)
                 for i = index then (incf index)
                 while (< i length)
                 do (setf (aref result i) ichars)
                 finally (when (< i length)
                           (push ichars eol-lines)))
        finally (setf (buffer-visible-bottom-line buffer) (or line prev-line))
                (return (values result eol-lines))))

;; FIXME: Support multi buffers (like split window)
;; (defun migrate-buffers ())

(defun migrate-buffer-line (ichars eol-p buffer window y start end)
  (loop with cursor = (buffer-cursor buffer)
        with cursor-ichar = (and cursor (bdl-ichar-ichar cursor))
        with win-width = (window-width window)
        for ichar across (ichars-with-padding ichars (- end start))
        for x from start below win-width
        when (and (eq buffer *current-buffer*)
                  cursor-ichar
                  (eq ichar cursor-ichar))
          do (setf (window-x window) x
                   (window-y window) y)
        do (set-window-ichar x y ichar)
        when (and ichar (> (ichar-width ichar) 1))
          do (incf x)
        finally (unless eol-p
                  (set-window-ichar x y (character-to-ichar #\\)))))

(defun buffer-status-line (buffer)
  (let* ((name (buffer-status buffer))
         (line (when name (make-line name))))
    (when line
      (bdl-ichars-to-ichars
       (line-top-bdl-ichar line)))))

(defun buffer-height-without-status-line (buffer)
  (if (buffer-status buffer)
      (1- (buffer-height buffer))
      (buffer-height buffer)))

(defun migrate-buffer (&optional (buffer *current-buffer*) (window *window*))
  (let* ((status-line (buffer-status-line buffer))
         (migrate-line-length (buffer-height-without-status-line buffer)))
    (multiple-value-bind (lines eol-lines)
        (buffer-board buffer migrate-line-length)
      (loop for ichars across lines
            for eol-p = (or (zerop (length ichars))
                            (and (find ichars eol-lines :test #'eq) t))
            for win-row from (buffer-position-y buffer)
            with win-col-start = (buffer-position-x buffer)
            with win-col-end = (+ win-col-start (buffer-width buffer))
            do (migrate-buffer-line ichars
                                    eol-p
                                    buffer
                                    window
                                    win-row
                                    win-col-start
                                    win-col-end)
            finally (when status-line
                      (migrate-buffer-line status-line
                                           t
                                           buffer
                                           window
                                           (1- (buffer-height buffer))
                                           win-col-start
                                           win-col-end))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redraw-buffer

;; FIXME: Support flags for should really redraw or not
;; redraw-all, redraw-line, no-need

;; FIXME: Support delay to redraw window

(defun redraw-buffer (&optional force-update (buffer *current-buffer*))
  (migrate-buffer buffer)
  (redraw force-update)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cursor

(defun prev-line (&optional (buffer *current-buffer*))
  (when (prev (buffer-visible-top-line buffer))
    (unless (bdl-index< (buffer-current-line buffer)
                        (buffer-visible-bottom-line buffer))
      (cursor-up buffer))
    (setf (buffer-visible-top-line buffer)
          (prev (buffer-visible-top-line buffer))
          (buffer-visible-bottom-line buffer)
          (prev (buffer-visible-bottom-line buffer)))
    (redraw-buffer nil buffer)))

(defun next-line (&optional (buffer *current-buffer*))
  (when (next (buffer-visible-bottom-line buffer))
    (when (bdl-index<= (buffer-current-line buffer)
                       (buffer-visible-top-line buffer))
      (cursor-down buffer))
    (setf (buffer-visible-top-line buffer)
          (next (buffer-visible-top-line buffer))
          (buffer-visible-bottom-line buffer)
          (next (buffer-visible-bottom-line buffer)))
    (redraw-buffer nil buffer)))

(defun cursor-up (&optional (buffer *current-buffer*))
  (flet ((decf-cursor-y ()
           (let* ((index (bdl-ichar-index (buffer-cursor buffer)))
                  (prev (prev (buffer-current-line buffer)))
                  (prev-top-bdl-ichar (line-top-bdl-ichar prev)))
             (setf (buffer-cursor buffer)
                   (or (get-by-index index prev-top-bdl-ichar)
                       (get-last prev-top-bdl-ichar)
                       prev-top-bdl-ichar)))))
    (prog1
        (cond
          ((bdl-index> (buffer-current-line buffer)
                       (buffer-visible-top-line buffer))
           (decf-cursor-y) t)
          ((prev-line)
           (decf-cursor-y) t)
          (t nil))
      (redraw-buffer nil buffer))))

(defun cursor-down (&optional (buffer *current-buffer*))
  (flet ((incf-cursor-y ()
           (let* ((index (bdl-ichar-index (buffer-cursor buffer)))
                  (next (next (buffer-current-line buffer)))
                  (next-top-bdl-ichar (line-top-bdl-ichar next)))
             (setf (buffer-cursor buffer)
                   (or (get-by-index index next-top-bdl-ichar)
                       (get-first next-top-bdl-ichar)
                       next-top-bdl-ichar)))))
    (prog1
        (cond
          (#1=(bdl-index< (buffer-current-line buffer)
                          (buffer-visible-bottom-line buffer))
              (incf-cursor-y) t)
          ((and (next-line buffer)
                #1#)
           (incf-cursor-y) t)
          (t nil))
      (redraw-buffer nil buffer))))

(defun cursor-left (&optional (buffer *current-buffer*))
  (when (prev (buffer-cursor buffer))
    (setf (buffer-cursor buffer)
          (prev (buffer-cursor buffer)))
    (redraw-buffer nil buffer)))

(defun cursor-right (&optional (buffer *current-buffer*))
  (when (next (buffer-cursor buffer))
    (setf (buffer-cursor buffer)
          (next (buffer-cursor buffer)))
    (redraw-buffer nil buffer)))

(defun cursor-left-most (&optional (buffer *current-buffer*))
  (setf (buffer-cursor buffer)
        (get-first (line-top-bdl-ichar (buffer-current-line buffer))))
  (redraw-buffer nil buffer))

(defun cursor-right-most (&optional (buffer *current-buffer*))
  (setf (buffer-cursor buffer)
        (get-last (line-top-bdl-ichar (buffer-current-line buffer))))
  (redraw-buffer nil buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete

(defun delete-line (&optional (buffer *current-buffer*))
  (let ((current-line (buffer-current-line buffer)))
    (cursor-down)
    (delete-bdl current-line)
    (redraw-buffer nil buffer)))

(defun delete-ichar-at-point (x y &optional (buffer *current-buffer*))
  (unless (zerop (length (buffer-top-line buffer)))
    (let* ((line (aref (buffer-top-line buffer) y))
           (ichars (line-ichars line)))
      (if (zerop (length ichars))
          (delete-line-at-point y buffer)
          (progn
            (delete-ichar-of-line x line)
            (normalize-x buffer)
            (redraw-buffer)
            t)))))

(defun delete-ichar (&optional (buffer *current-buffer*))
  (delete-ichar-at-point (buffer-x buffer) (buffer-y buffer) buffer))

(defun delete-prev-ichar (&optional (buffer *current-buffer*))
  (when (delete-ichar-at-point (max (1- (buffer-x buffer)) 0)
                               (buffer-y buffer)
                               buffer)
    (when (and (> (buffer-x buffer) 0)
               (< (buffer-x buffer) (buffer-x-max buffer)))
      (decf (buffer-x buffer))
      (redraw-buffer))
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace

(defun ensure-buffer-has-more-than-one-lines (buffer)
  (when (zerop (length (buffer-top-line buffer)))
    (setf (buffer-top-line buffer) (vector (make-line nil)))))

(defun replace-ichar-at-point (ichar x y &optional (buffer *current-buffer*))
  (ensure-buffer-has-more-than-one-lines buffer)
  (replace-ichar-at-point ichar x (aref (buffer-top-line buffer) y))
  (redraw-buffer nil buffer)
  t)

(defun replace-ichar (ichar &optional (buffer *current-buffer*))
  (replace-ichar-at-point ichar (buffer-x buffer) (buffer-y buffer) buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert

(defun insert-new-line-at-point (y &optional (buffer *current-buffer*))
  (ensure-buffer-has-more-than-one-lines buffer)
  (let ((lines (buffer-top-line buffer)))
    (setf (buffer-top-line buffer)
          (concatenate 'vector
                       (subseq lines 0 y)
                       (vector (make-line nil))
                       (subseq lines y))))
  (normalize-x buffer)
  (redraw-buffer)
  t)

(defun insert-new-line (&optional (buffer *current-buffer*))
  (insert-new-line-at-point (buffer-y buffer) buffer))

(defun insert-next-line (&optional (buffer *current-buffer*))
  (insert-new-line-at-point (1+ (buffer-y buffer)) buffer))

(defun insert-eol-at-point (x y &optional (buffer *current-buffer*))
  (ensure-buffer-has-more-than-one-lines buffer)
  (let* ((lines (buffer-top-line buffer))
         (ichars (line-ichars (aref lines y))))
    (setf (buffer-top-line buffer)
          (concatenate 'vector
                       (subseq lines 0 y)
                       (list (make-line (subseq ichars 0 x) t)
                             (make-line (subseq ichars x) t))
                       (subseq lines (1+ y))))
    (incf (buffer-y buffer))
    (setf (buffer-x buffer) 0)
    (redraw-buffer)
    t))

(defun insert-eol (&optional (buffer *current-buffer*))
  (insert-eol-at-point (buffer-x buffer) (buffer-y buffer) buffer))

(defun insert-ichar-at-point (ichar x y &optional (buffer *current-buffer*))
  (ensure-buffer-has-more-than-one-lines buffer)
  (insert-ichar-to-line ichar x (aref (buffer-top-line buffer) y))
  (redraw-buffer)
  t)

(defun insert-ichar (ichar &optional (buffer *current-buffer*))
  (insert-ichar-at-point ichar (buffer-x buffer) (buffer-y buffer) buffer))
