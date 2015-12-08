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
           :buffer-status
           :buffer-top-line
           :buffer-cursor-position
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
;; buffer-has-at-least-one-line-p

(defun buffer-has-at-least-one-line-p (buffer)
  (has-at-least-one-p (buffer-top-line buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-cursor-position

(defun buffer-cursor-x (buffer)
  (let ((cursor (buffer-cursor buffer)))
    (if (or (not cursor)
            (top-bdl-p cursor))
        0
        (bdl-ichar-index cursor))))

(defun buffer-cursor-y (buffer)
  (if (buffer-cursor buffer)
      (line-index (buffer-current-line buffer))
      0))

(defun buffer-cursor-position (buffer)
  (cons (buffer-cursor-x buffer)
        (buffer-cursor-y buffer)))


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
  (when (and *window* (slot-boundp buffer 'lines))
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
      (setf (buffer-cursor buffer) nil
            (buffer-visible-top-line buffer) nil)
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
        with cursor = (buffer-cursor buffer)
        with cursor-ichar = (when cursor (bdl-ichar-ichar cursor))
        with position = (unless cursor (cons 0 0))
        for index from 0 below length
        for prev-line = nil then line
        for line = (buffer-visible-top-line buffer) then (next line)
        while line
        when (eq cursor (line-top-bdl-ichar line))
          do (setq position (cons 0 index))
        do (loop for ichars
                   in (fold-ichars (bdl-ichars-to-ichars
                                    (line-top-bdl-ichar line))
                                   buffer)
                 for i = index then (incf index)
                 while (< i length)
                 do (setf (aref result i) ichars)
                 when cursor-ichar
                   do (loop for ichar across ichars
                            for x from 0
                            when (eq cursor-ichar ichar)
                              do (setq position (cons x i)))
                 finally (when (< i length)
                           (push ichars eol-lines)))
        finally (setf (buffer-visible-bottom-line buffer) (or line prev-line))
                (return (values result eol-lines position))))

;; FIXME: Support multi buffers (like split window)
;; (defun migrate-buffers ())

(defun migrate-buffer-line (ichars eol-p window y start end)
  (loop with win-width = (window-width window)
        for ichar across (ichars-with-padding ichars (- end start))
        for x from start below win-width
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
    (multiple-value-bind (lines eol-lines position)
        (buffer-board buffer migrate-line-length)
      (setf (window-x window) (+ (car position)
                                 (buffer-position-x buffer))
            (window-y window) (+ (cdr position)
                                 (buffer-position-y buffer)))
      (loop for ichars across lines
            for eol-p = (or (zerop (length ichars))
                            (and (find ichars eol-lines :test #'eq) t))
            for win-row from (buffer-position-y buffer)
            with win-col-start = (buffer-position-x buffer)
            with win-col-end = (+ win-col-start (buffer-width buffer))
            do (migrate-buffer-line ichars
                                    eol-p
                                    window
                                    win-row
                                    win-col-start
                                    win-col-end)
            finally (when status-line
                      (migrate-buffer-line status-line
                                           t
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

(defun visible-bottom-p (buffer)
  (eq (buffer-current-line buffer)
      (buffer-visible-bottom-line buffer)))

(defun visible-top-p (buffer)
  (eq (buffer-current-line buffer)
      (buffer-visible-top-line buffer)))

(defun not-on-the-edge-p (buffer)
  (and (not (visible-bottom-p buffer))
       (not (visible-top-p buffer))))

(defun prev-line (&optional (buffer *current-buffer*))
  (when (prev (buffer-visible-top-line buffer))
    (when (visible-bottom-p buffer)
      (cursor-up buffer))
    (setf (buffer-visible-top-line buffer)
          (prev (buffer-visible-top-line buffer))
          (buffer-visible-bottom-line buffer)
          (prev (buffer-visible-bottom-line buffer)))
    (redraw-buffer nil buffer)))

(defun next-line (&optional (buffer *current-buffer*))
  (when (next (buffer-visible-bottom-line buffer))
    (when (visible-top-p buffer)
      (cursor-down buffer))
    (setf (buffer-visible-top-line buffer)
          (next (buffer-visible-top-line buffer))
          (buffer-visible-bottom-line buffer)
          (next (buffer-visible-bottom-line buffer)))
    (redraw-buffer nil buffer)))

(defun cursor-up (&optional (buffer *current-buffer*))
  (flet ((decf-cursor-y ()
           (let* ((index (buffer-cursor-x buffer))
                  (prev (prev (buffer-current-line buffer)))
                  (prev-top-bdl-ichar (line-top-bdl-ichar prev)))
             (setf (buffer-cursor buffer)
                   (or (get-by-index index prev-top-bdl-ichar)
                       (get-last prev-top-bdl-ichar)
                       prev-top-bdl-ichar)))))
    (when (buffer-cursor buffer)
      (prog1
          (cond
            ((not-on-the-edge-p buffer)
             (decf-cursor-y) t)
            ((prev-line)
             (decf-cursor-y) t)
            (t nil))
        (redraw-buffer nil buffer)))))

(defun cursor-down (&optional (buffer *current-buffer*))
  (flet ((incf-cursor-y ()
           (let* ((index (buffer-cursor-x buffer))
                  (next (next (buffer-current-line buffer)))
                  (next-top-bdl-ichar (line-top-bdl-ichar next)))
             (setf (buffer-cursor buffer)
                   (or (get-by-index index next-top-bdl-ichar)
                       (get-last next-top-bdl-ichar)
                       next-top-bdl-ichar)))))
    (when (buffer-cursor buffer)
      (prog1
          (cond
            ((not-on-the-edge-p buffer)
             (incf-cursor-y) t)
            ((and (next-line buffer)
                  (not-on-the-edge-p buffer))
             (incf-cursor-y) t)
            (t nil))
        (redraw-buffer nil buffer)))))

(defun cursor-left (&optional (buffer *current-buffer*))
  (when (and (buffer-has-at-least-one-line-p buffer)
             (prev (buffer-cursor buffer)))
    (setf (buffer-cursor buffer)
          (prev (buffer-cursor buffer)))
    (redraw-buffer nil buffer)
    t))

(defun cursor-right (&optional (buffer *current-buffer*))
  (when (and (buffer-has-at-least-one-line-p buffer)
             (next (buffer-cursor buffer)))
    (setf (buffer-cursor buffer)
          (next (buffer-cursor buffer)))
    (redraw-buffer nil buffer)
    t))

(defun cursor-left-most (&optional (buffer *current-buffer*))
  (when (buffer-has-at-least-one-line-p buffer)
    (setf (buffer-cursor buffer)
          (get-first (line-top-bdl-ichar (buffer-current-line buffer))))
    (redraw-buffer nil buffer)
    t))

(defun cursor-right-most (&optional (buffer *current-buffer*))
  (when (buffer-has-at-least-one-line-p buffer)
    (setf (buffer-cursor buffer)
          (get-last (line-top-bdl-ichar (buffer-current-line buffer))))
    (redraw-buffer nil buffer)
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete

(defun delete-line (&optional (buffer *current-buffer*))
  (when (buffer-has-at-least-one-line-p buffer)
    (let ((current-line (buffer-current-line buffer)))
      (cond
        ((eq current-line (buffer-visible-top-line buffer))
         (next-line buffer))
        ((eq current-line (buffer-visible-bottom-line buffer))
         (cursor-up buffer))
        (t (cursor-down buffer)))
      (delete-bdl current-line)
      (when (eq current-line (buffer-visible-bottom-line buffer))
        (cursor-down buffer))
      (redraw-buffer nil buffer)
      t)))

(defun delete-ichar (&optional (buffer *current-buffer*))
  (when (buffer-has-at-least-one-line-p buffer)
    (let ((cursor (buffer-cursor buffer)))
      (cond
        ((or (cursor-right)
             (cursor-left))
         (delete-bdl cursor))
        ((not (top-bdl-p cursor))
         (setf (buffer-cursor buffer)
               (line-top-bdl-ichar (buffer-current-line buffer)))
         (delete-bdl cursor)))
      (redraw-buffer nil buffer)
      t)))

(defun delete-prev-ichar (&optional (buffer *current-buffer*))
  (when (buffer-has-at-least-one-line-p buffer)
    (let ((prev (prev (buffer-cursor buffer))))
      (when prev
        (delete-bdl prev)
        (redraw-buffer nil buffer)
        t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace

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
