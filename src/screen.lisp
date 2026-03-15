;;; screen.lisp - Double-buffered screen rendering for Charmed
;;; Maintains off-screen buffer and diffs to minimize terminal output

(in-package #:charmed)

;;; ============================================================
;;; Cell Structure
;;; ============================================================

(defstruct (cell (:constructor make-cell (&optional (char #\Space) fg bg style)))
  "A single terminal cell with character and attributes."
  (char #\Space :type character)
  (fg nil :type (or null named-color indexed-color rgb-color))
  (bg nil :type (or null named-color indexed-color rgb-color))
  (style nil :type (or null text-style)))

(defun cell-equal (a b)
  "Compare two cells for equality."
  (and (eql (cell-char a) (cell-char b))
       (equalp (cell-fg a) (cell-fg b))
       (equalp (cell-bg a) (cell-bg b))
       (equalp (cell-style a) (cell-style b))))

;;; ============================================================
;;; Screen Buffer
;;; ============================================================

(defclass screen-buffer ()
  ((width :initarg :width :accessor buffer-width :initform 80)
   (height :initarg :height :accessor buffer-height :initform 24)
   (cells :accessor buffer-cells :initform nil
          :documentation "2D array of cells (row-major)")
   (cursor-x :initarg :cursor-x :accessor buffer-cursor-x :initform 1)
   (cursor-y :initarg :cursor-y :accessor buffer-cursor-y :initform 1)
   (cursor-visible :initarg :cursor-visible :accessor buffer-cursor-visible :initform t))
  (:documentation "Off-screen buffer for double-buffered rendering."))

(defmethod initialize-instance :after ((buf screen-buffer) &key)
  "Initialize the cell array."
  (let ((w (buffer-width buf))
        (h (buffer-height buf)))
    (setf (buffer-cells buf)
          (make-array (list h w) :initial-element nil))
    (dotimes (row h)
      (dotimes (col w)
        (setf (aref (buffer-cells buf) row col) (make-cell))))))

(defun buffer-clear (buf &optional (char #\Space))
  "Clear buffer to empty cells."
  (let ((cells (buffer-cells buf)))
    (dotimes (row (buffer-height buf))
      (dotimes (col (buffer-width buf))
        (let ((cell (aref cells row col)))
          (setf (cell-char cell) char
                (cell-fg cell) nil
                (cell-bg cell) nil
                (cell-style cell) nil))))))

(defun buffer-resize (buf new-width new-height)
  "Resize buffer, preserving content where possible."
  (let* ((old-cells (buffer-cells buf))
         (old-w (buffer-width buf))
         (old-h (buffer-height buf))
         (new-cells (make-array (list new-height new-width) :initial-element nil)))
    ;; Initialize new cells
    (dotimes (row new-height)
      (dotimes (col new-width)
        (if (and (< row old-h) (< col old-w))
            (setf (aref new-cells row col) (aref old-cells row col))
            (setf (aref new-cells row col) (make-cell)))))
    (setf (buffer-width buf) new-width
          (buffer-height buf) new-height
          (buffer-cells buf) new-cells)))

(defun buffer-set-cell (buf x y char &key fg bg style)
  "Set cell at (x, y) - 1-indexed coordinates.
   All fields are always set — passing NIL for FG, BG, or STYLE clears
   any previous value on the cell."
  (when (and (>= x 1) (<= x (buffer-width buf))
             (>= y 1) (<= y (buffer-height buf)))
    (let ((cell (aref (buffer-cells buf) (1- y) (1- x))))
      (setf (cell-char cell) char
            (cell-fg cell) fg
            (cell-bg cell) bg
            (cell-style cell) style))))

(defun buffer-get-cell (buf x y)
  "Get cell at (x, y) - 1-indexed coordinates."
  (when (and (>= x 1) (<= x (buffer-width buf))
             (>= y 1) (<= y (buffer-height buf)))
    (aref (buffer-cells buf) (1- y) (1- x))))

(defun buffer-write-string (buf x y string &key fg bg style)
  "Write STRING starting at (x, y)."
  (loop for i from 0 below (length string)
        for col = (+ x i)
        while (<= col (buffer-width buf))
        do (buffer-set-cell buf col y (char string i) :fg fg :bg bg :style style)))

(defun buffer-fill-rect (buf x y width height &key (char #\Space) fg bg style)
  "Fill a rectangle with the given character and attributes."
  (loop for row from y below (+ y height)
        while (<= row (buffer-height buf))
        do (loop for col from x below (+ x width)
                 while (<= col (buffer-width buf))
                 do (buffer-set-cell buf col row char :fg fg :bg bg :style style))))

;;; ============================================================
;;; Screen (Double Buffer Manager)
;;; ============================================================

(defclass screen ()
  ((front :accessor screen-front :initform nil
          :documentation "Front buffer (what's currently displayed)")
   (back :accessor screen-back :initform nil
         :documentation "Back buffer (what we're drawing to)")
   (width :initarg :width :accessor screen-width :initform 80)
   (height :initarg :height :accessor screen-height :initform 24)
   (stream :initarg :stream :accessor screen-stream :initform *terminal-io*)
   (full-redraw :accessor screen-full-redraw :initform t
                :documentation "Force full redraw on next present"))
  (:documentation "Double-buffered screen manager."))

(defmethod initialize-instance :after ((scr screen) &key)
  "Initialize front and back buffers."
  (let ((w (screen-width scr))
        (h (screen-height scr)))
    (setf (screen-front scr) (make-instance 'screen-buffer :width w :height h))
    (setf (screen-back scr) (make-instance 'screen-buffer :width w :height h))))

(defun screen-resize (scr new-width new-height)
  "Resize screen and both buffers."
  (setf (screen-width scr) new-width
        (screen-height scr) new-height)
  (buffer-resize (screen-front scr) new-width new-height)
  (buffer-resize (screen-back scr) new-width new-height)
  (setf (screen-full-redraw scr) t))

(defun screen-clear (scr)
  "Clear the back buffer."
  (buffer-clear (screen-back scr)))

(defun screen-set-cell (scr x y char &key fg bg style)
  "Set cell in back buffer."
  (buffer-set-cell (screen-back scr) x y char :fg fg :bg bg :style style))

(defun screen-write-string (scr x y string &key fg bg style)
  "Write string to back buffer."
  (buffer-write-string (screen-back scr) x y string :fg fg :bg bg :style style))

(defun screen-fill-rect (scr x y width height &key (char #\Space) fg bg style)
  "Fill rectangle in back buffer."
  (buffer-fill-rect (screen-back scr) x y width height :char char :fg fg :bg bg :style style))

(defun screen-set-cursor (scr x y)
  "Set cursor position in back buffer."
  (setf (buffer-cursor-x (screen-back scr)) x
        (buffer-cursor-y (screen-back scr)) y))

(defun screen-show-cursor (scr visible)
  "Set cursor visibility in back buffer."
  (setf (buffer-cursor-visible (screen-back scr)) visible))

;;; ============================================================
;;; Differential Rendering
;;; ============================================================

(defun emit-unapply-style (style stream)
  "Emit ANSI codes to turn OFF attributes that were set in STYLE.
   Resets fg/bg colors if the style had set them."
  (when style
    (when (style-bold-p style) (format stream "~C[22m" *escape*))
    (when (style-dim-p style) (format stream "~C[22m" *escape*))
    (when (style-italic-p style) (format stream "~C[23m" *escape*))
    (when (style-underline-p style) (format stream "~C[24m" *escape*))
    (when (style-inverse-p style) (format stream "~C[27m" *escape*))
    (when (style-fg style) (format stream "~C[39m" *escape*))
    (when (style-bg style) (format stream "~C[49m" *escape*))))

(defun emit-cell-style (stream cell last-fg last-bg last-style)
  "Emit style codes for cell, return new fg/bg/style state."
  (let ((fg (cell-fg cell))
        (bg (cell-bg cell))
        (style (cell-style cell))
        (changed nil))
    ;; Turn off previous style attributes when transitioning styled→unstyled
    ;; or when style changed (different style object)
    (when (and last-style
               (not (equalp last-style style)))
      (emit-unapply-style last-style stream)
      (setf changed t))
    ;; Check if we need to emit anything
    (when (or (not (equalp fg last-fg))
              (not (equalp bg last-bg))
              style)
      (setf changed t)
      (when style
        (emit-style style stream))
      (when (not (equalp fg last-fg))
        (if fg
            (fg-color fg stream)
            ;; fg is nil but last-fg was set — reset to default foreground
            (format stream "~C[39m" *escape*)))
      (when (not (equalp bg last-bg))
        (if bg
            (bg-color bg stream)
            ;; bg is nil but last-bg was set — reset to default background
            (format stream "~C[49m" *escape*))))
    (values fg bg style changed)))

(defun screen-present (scr)
  "Present back buffer to terminal, only updating changed cells.
   Optimizations:
   - Skip unchanged cells
   - Batch consecutive characters on same row
   - Minimize cursor movement commands
   - Track attribute state to avoid redundant escapes"
  (let* ((stream (screen-stream scr))
         (front (screen-front scr))
         (back (screen-back scr))
         (width (screen-width scr))
         (height (screen-height scr))
         (full-redraw (screen-full-redraw scr))
         (front-cells (buffer-cells front))
         (back-cells (buffer-cells back))
         (last-x nil)
         (last-y nil)
         (last-fg nil)
         (last-bg nil)
         (last-style nil)
         (cells-updated 0))
    ;; Hide cursor during update
    (hide-cursor stream)
    ;; Scan for differences
    (dotimes (row height)
      (dotimes (col width)
        (let ((front-cell (aref front-cells row col))
              (back-cell (aref back-cells row col)))
          (when (or full-redraw (not (cell-equal front-cell back-cell)))
            (incf cells-updated)
            ;; Move cursor if not sequential
            (let ((x (1+ col))
                  (y (1+ row)))
              (unless (and last-x last-y
                           (= y last-y)
                           (= x (1+ last-x)))
                ;; Non-sequential: unapply style before jumping so it
                ;; doesn't bleed across skipped cells
                (when last-style
                  (emit-unapply-style last-style stream)
                  (setf last-style nil))
                (cursor-to y x stream))
              ;; Emit style if needed
              (multiple-value-bind (new-fg new-bg new-style changed)
                  (emit-cell-style stream back-cell last-fg last-bg last-style)
                (declare (ignore changed))
                (setf last-fg new-fg last-bg new-bg last-style new-style))
              ;; Write character
              (write-char (cell-char back-cell) stream)
              ;; Copy to front buffer
              (setf (cell-char front-cell) (cell-char back-cell)
                    (cell-fg front-cell) (cell-fg back-cell)
                    (cell-bg front-cell) (cell-bg back-cell)
                    (cell-style front-cell) (cell-style back-cell))
              (setf last-x x last-y y))))))
    ;; Reset attributes only if we changed something
    (when (> cells-updated 0)
      (reset stream))
    ;; Position cursor
    (let ((cx (buffer-cursor-x back))
          (cy (buffer-cursor-y back)))
      (cursor-to cy cx stream)
      (setf (buffer-cursor-x front) cx
            (buffer-cursor-y front) cy))
    ;; Show/hide cursor
    (if (buffer-cursor-visible back)
        (show-cursor stream)
        (hide-cursor stream))
    (setf (buffer-cursor-visible front) (buffer-cursor-visible back))
    ;; Clear full redraw flag
    (setf (screen-full-redraw scr) nil)
    ;; Flush
    (force-output stream)
    ;; Return count of updated cells for debugging
    cells-updated))

(defun screen-force-redraw (scr)
  "Force a full redraw on next present."
  (setf (screen-full-redraw scr) t))

;;; ============================================================
;;; Global Screen Instance
;;; ============================================================

(defvar *screen* nil
  "Global screen instance for double-buffered rendering.")

(defun init-screen (&optional (width 80) (height 24))
  "Initialize the global screen."
  (setf *screen* (make-instance 'screen :width width :height height))
  (clear-screen)
  (cursor-to 1 1)
  *screen*)

(defun term-screen ()
  "Terminate the screen and restore terminal."
  (when *screen*
    (reset)
    (show-cursor)
    (clear-screen)
    (cursor-to 1 1)
    (force-output *terminal-io*)
    (setf *screen* nil)))

(defun with-screen-macro-helper (width height body-fn)
  "Helper for with-screen macro."
  (unwind-protect
       (progn
         (init-screen width height)
         (funcall body-fn))
    (term-screen)))

(defmacro with-screen ((&key (width 80) (height 24)) &body body)
  "Execute BODY with an initialized screen, cleaning up afterward."
  `(with-screen-macro-helper ,width ,height (lambda () ,@body)))

;;; ============================================================
;;; Output Buffering for Performance
;;; ============================================================

(defvar *output-buffer* nil
  "String output stream for batching terminal output.")

(defvar *output-buffer-string* nil
  "The underlying string for the output buffer.")

(defun init-output-buffer (&optional (initial-size 4096))
  "Initialize the output buffer."
  (setf *output-buffer-string* (make-array initial-size 
                                           :element-type 'character
                                           :fill-pointer 0
                                           :adjustable t))
  (setf *output-buffer* (make-string-output-stream)))

(defun flush-output-buffer (&optional (stream *terminal-io*))
  "Flush buffered output to stream."
  (when *output-buffer*
    (let ((str (get-output-stream-string *output-buffer*)))
      (when (> (length str) 0)
        (write-string str stream)
        (force-output stream)))))

(defun clear-output-buffer ()
  "Clear the output buffer without flushing."
  (when *output-buffer*
    (get-output-stream-string *output-buffer*)))

(defmacro with-buffered-output ((&optional (stream '*terminal-io*)) &body body)
  "Execute BODY with output buffered, then flush to STREAM.
   This reduces system calls by batching escape sequences."
  (let ((old-buffer (gensym "OLD-BUFFER"))
        (result (gensym "RESULT")))
    `(let ((,old-buffer *output-buffer*))
       (unwind-protect
            (progn
              (init-output-buffer)
              (let ((*terminal-io* *output-buffer*)
                    (,result nil))
                (setf ,result (progn ,@body))
                (flush-output-buffer ,stream)
                ,result))
         (setf *output-buffer* ,old-buffer)))))

;;; ============================================================
;;; Accessibility Support
;;; ============================================================
;;; These functions emit hints that screen readers may use.
;;; Based on ARIA-like concepts adapted for terminal environments.

(defvar *accessibility-enabled* nil
  "When T, emit accessibility hints for screen readers.")

(defun enable-accessibility ()
  "Enable accessibility hints."
  (setf *accessibility-enabled* t))

(defun disable-accessibility ()
  "Disable accessibility hints."
  (setf *accessibility-enabled* nil))

(defun announce (message &optional (stream *terminal-io*))
  "Announce MESSAGE to screen reader (using bell + message pattern).
   Some terminal screen readers detect text following bell as announcements."
  (when *accessibility-enabled*
    ;; Use OSC 777 for notifications (supported by some terminals)
    (format stream "~C]777;notify;Charmed;~A~C\\" #\Escape message #\Escape)
    (force-output stream)))

(defun set-title (title &optional (stream *terminal-io*))
  "Set terminal window title (helps screen reader users identify window)."
  ;; OSC 0 - Set window title
  (format stream "~C]0;~A~C\\" #\Escape title #\Escape)
  (force-output stream))

(defun set-region-name (name &optional (stream *terminal-io*))
  "Set a named region hint (for screen reader navigation).
   Uses OSC 1337 iTerm2-style annotation."
  (when *accessibility-enabled*
    (format stream "~C]1337;SetMark~C\\" #\Escape #\Escape)
    (format stream "~C]1337;Annotation=~A~C\\" #\Escape name #\Escape)
    (force-output stream)))

(defun widget-focus-hint (widget-type widget-label &optional (stream *terminal-io*))
  "Emit a focus hint for a widget."
  (when *accessibility-enabled*
    (announce (format nil "~A: ~A" widget-type widget-label) stream)))

(defun selection-hint (item-text position total &optional (stream *terminal-io*))
  "Emit a selection hint (e.g., for list navigation)."
  (when *accessibility-enabled*
    (announce (format nil "~A (~D of ~D)" item-text position total) stream)))

(defun action-hint (action &optional (stream *terminal-io*))
  "Emit an action hint (e.g., 'expanded', 'collapsed', 'selected')."
  (when *accessibility-enabled*
    (announce action stream)))
