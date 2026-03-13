;;; widgets.lisp - Generic widget classes for CLansi
;;; Base panel class and scrollable list - no app-specific widgets

(in-package #:clansi)

;;; ============================================================
;;; Base Panel Class
;;; ============================================================

(defclass panel ()
  ((x :initarg :x :accessor panel-x :initform 1
      :documentation "X position (1-indexed column)")
   (y :initarg :y :accessor panel-y :initform 1
      :documentation "Y position (1-indexed row)")
   (width :initarg :width :accessor panel-width :initform 40)
   (height :initarg :height :accessor panel-height :initform 10)
   (title :initarg :title :accessor panel-title :initform nil
          :documentation "Optional title displayed in border")
   (active-p :initarg :active :accessor panel-active-p :initform nil
             :documentation "Whether panel is currently focused")
   (visible-p :initarg :visible :accessor panel-visible-p :initform t
              :documentation "Whether panel should be rendered")
   (border-p :initarg :border :accessor panel-border-p :initform t
             :documentation "Whether to draw a border")
   (dirty-p :initform t :accessor panel-dirty-p
            :documentation "Whether panel needs redraw"))
  (:documentation "Base class for UI panels. Subclass and override panel-render."))

(defgeneric panel-render (panel)
  (:documentation "Render the panel to the terminal."))

(defgeneric panel-clear (panel)
  (:documentation "Clear the panel area."))

(defgeneric panel-content-x (panel)
  (:documentation "X coordinate of content area (inside border)."))

(defgeneric panel-content-y (panel)
  (:documentation "Y coordinate of content area (inside border)."))

(defgeneric panel-content-width (panel)
  (:documentation "Width of content area (inside border)."))

(defgeneric panel-content-height (panel)
  (:documentation "Height of content area (inside border)."))

(defmethod panel-content-x ((panel panel))
  (if (panel-border-p panel)
      (1+ (panel-x panel))
      (panel-x panel)))

(defmethod panel-content-y ((panel panel))
  (if (panel-border-p panel)
      (1+ (panel-y panel))
      (panel-y panel)))

(defmethod panel-content-width ((panel panel))
  (if (panel-border-p panel)
      (- (panel-width panel) 2)
      (panel-width panel)))

(defmethod panel-content-height ((panel panel))
  (if (panel-border-p panel)
      (- (panel-height panel) 2)
      (panel-height panel)))

(defmethod panel-clear ((panel panel))
  "Clear the panel's content area."
  (let ((cx (panel-content-x panel))
        (cy (panel-content-y panel))
        (cw (panel-content-width panel))
        (ch (panel-content-height panel)))
    (fill-rect cx cy cw ch #\Space)))

(defmethod panel-render ((panel panel))
  "Default render: draw border if enabled, clear content area."
  (when (panel-visible-p panel)
    (when (panel-border-p panel)
      (render-panel-border panel))
    (panel-clear panel)
    (setf (panel-dirty-p panel) nil)))

(defun render-panel-border (panel &key (border-style nil)
                                       (active-style nil)
                                       (tl #\┌) (tr #\┐) (bl #\└) (br #\┘)
                                       (h #\─) (v #\│))
  "Render the border for a panel with optional styling."
  (let* ((x (panel-x panel))
         (y (panel-y panel))
         (w (panel-width panel))
         (height (panel-height panel))
         (title (panel-title panel))
         (style (if (panel-active-p panel) active-style border-style)))
    (when style (emit-style style *terminal-io*))
    ;; Top border with optional title
    (cursor-to y x)
    (princ tl *terminal-io*)
    (if title
        (let* ((max-title-w (- w 4))
               (display-title (if (> (length title) max-title-w)
                                  (subseq title 0 max-title-w)
                                  title)))
          (princ h *terminal-io*)
          (when (panel-active-p panel) (bold))
          (princ display-title *terminal-io*)
          (when (panel-active-p panel) (reset) (when style (emit-style style *terminal-io*)))
          (loop repeat (- w 2 1 (length display-title))
                do (princ h *terminal-io*)))
        (loop repeat (- w 2) do (princ h *terminal-io*)))
    (princ tr *terminal-io*)
    ;; Side borders
    (loop for row from (1+ y) below (+ y height -1) do
      (cursor-to row x)
      (princ v *terminal-io*)
      (cursor-to row (+ x w -1))
      (princ v *terminal-io*))
    ;; Bottom border
    (cursor-to (+ y height -1) x)
    (princ bl *terminal-io*)
    (loop repeat (- w 2) do (princ h *terminal-io*))
    (princ br *terminal-io*)
    (when style (reset))))

;;; ============================================================
;;; Scrollable List Widget
;;; ============================================================

(defclass scrollable-list (panel)
  ((items :initarg :items :accessor list-items :initform nil
          :documentation "List of items to display")
   (cursor :initarg :cursor :accessor list-cursor :initform 0
           :documentation "Currently selected item index (0-indexed)")
   (scroll-offset :initarg :scroll-offset :accessor list-scroll-offset :initform 0
                  :documentation "First visible item index")
   (item-renderer :initarg :item-renderer :accessor list-item-renderer
                  :initform #'princ
                  :documentation "Function to render each item: (item index selected-p width)"))
  (:documentation "A scrollable list of items with cursor selection."))

(defgeneric list-visible-height (list)
  (:documentation "Number of items visible in the list."))

(defgeneric list-move-cursor (list direction)
  (:documentation "Move cursor up/down, handling scrolling."))

(defgeneric list-selected-item (list)
  (:documentation "Return the currently selected item."))

(defmethod list-visible-height ((list scrollable-list))
  (panel-content-height list))

(defmethod list-selected-item ((list scrollable-list))
  (let ((items (list-items list))
        (cursor (list-cursor list)))
    (when (and items (>= cursor 0) (< cursor (length items)))
      (nth cursor items))))

(defmethod list-move-cursor ((list scrollable-list) direction)
  "Move cursor by DIRECTION (+1 for down, -1 for up).
   Handles bounds checking and scroll adjustment."
  (let* ((items (list-items list))
         (count (length items))
         (cursor (list-cursor list))
         (visible (list-visible-height list))
         (new-cursor (+ cursor direction)))
    (when (and (>= new-cursor 0) (< new-cursor count))
      (setf (list-cursor list) new-cursor)
      ;; Adjust scroll if cursor moved out of view
      (let ((scroll (list-scroll-offset list)))
        (cond
          ;; Cursor above visible area
          ((< new-cursor scroll)
           (setf (list-scroll-offset list) new-cursor))
          ;; Cursor below visible area
          ((>= new-cursor (+ scroll visible))
           (setf (list-scroll-offset list) (1+ (- new-cursor visible))))))
      (setf (panel-dirty-p list) t))))

(defmethod panel-render ((list scrollable-list))
  "Render the scrollable list with items."
  (when (panel-visible-p list)
    ;; Draw border
    (when (panel-border-p list)
      (render-panel-border list))
    ;; Render items
    (let* ((cx (panel-content-x list))
           (cy (panel-content-y list))
           (cw (panel-content-width list))
           (ch (panel-content-height list))
           (items (list-items list))
           (scroll (list-scroll-offset list))
           (cursor (list-cursor list))
           (renderer (list-item-renderer list)))
      ;; Clear content area first
      (fill-rect cx cy cw ch #\Space)
      ;; Render visible items
      (loop for row from 0 below ch
            for idx = (+ scroll row)
            while (< idx (length items))
            do (let* ((item (nth idx items))
                      (selected-p (= idx cursor)))
                 (cursor-to (+ cy row) cx)
                 (when selected-p (inverse))
                 (funcall renderer item idx selected-p cw)
                 (when selected-p (reset)))))
    (setf (panel-dirty-p list) nil)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun truncate-string (string max-length &optional (ellipsis "…"))
  "Truncate STRING to MAX-LENGTH, adding ELLIPSIS if truncated."
  (if (<= (length string) max-length)
      string
      (concatenate 'string 
                   (subseq string 0 (- max-length (length ellipsis)))
                   ellipsis)))

(defun pad-string (string width &optional (pad-char #\Space))
  "Pad STRING to WIDTH with PAD-CHAR on the right."
  (let ((len (length string)))
    (if (>= len width)
        (subseq string 0 width)
        (concatenate 'string string 
                     (make-string (- width len) :initial-element pad-char)))))

;;; ============================================================
;;; Text Input Widget
;;; ============================================================

(defclass text-input (panel)
  ((value :initarg :value :accessor input-value :initform ""
          :documentation "Current text value")
   (cursor-pos :initarg :cursor-pos :accessor input-cursor-pos :initform 0
               :documentation "Cursor position within the text")
   (scroll-offset :initarg :scroll-offset :accessor input-scroll-offset :initform 0
                  :documentation "Horizontal scroll offset for long text")
   (placeholder :initarg :placeholder :accessor input-placeholder :initform ""
                :documentation "Placeholder text shown when empty")
   (max-length :initarg :max-length :accessor input-max-length :initform nil
               :documentation "Maximum allowed text length (nil = unlimited)")
   (password-p :initarg :password :accessor input-password-p :initform nil
               :documentation "If true, display asterisks instead of text"))
  (:default-initargs :height 1 :border nil)
  (:documentation "Single-line text input field with cursor and editing."))

(defgeneric input-insert-char (input char)
  (:documentation "Insert a character at the cursor position."))

(defgeneric input-delete-char (input)
  (:documentation "Delete character before cursor (backspace)."))

(defgeneric input-delete-forward (input)
  (:documentation "Delete character at cursor (delete key)."))

(defgeneric input-move-cursor (input direction)
  (:documentation "Move cursor left (-1) or right (+1)."))

(defgeneric input-move-to-start (input)
  (:documentation "Move cursor to start of input."))

(defgeneric input-move-to-end (input)
  (:documentation "Move cursor to end of input."))

(defgeneric input-clear (input)
  (:documentation "Clear all text from input."))

(defgeneric input-handle-key (input key-event)
  (:documentation "Handle a key event, return T if handled."))

(defmethod input-insert-char ((input text-input) char)
  "Insert CHAR at cursor position."
  (let* ((value (input-value input))
         (pos (input-cursor-pos input))
         (max-len (input-max-length input)))
    (when (or (null max-len) (< (length value) max-len))
      (setf (input-value input)
            (concatenate 'string
                         (subseq value 0 pos)
                         (string char)
                         (subseq value pos)))
      (incf (input-cursor-pos input))
      (setf (panel-dirty-p input) t))))

(defmethod input-delete-char ((input text-input))
  "Delete character before cursor (backspace)."
  (let* ((value (input-value input))
         (pos (input-cursor-pos input)))
    (when (> pos 0)
      (setf (input-value input)
            (concatenate 'string
                         (subseq value 0 (1- pos))
                         (subseq value pos)))
      (decf (input-cursor-pos input))
      (setf (panel-dirty-p input) t))))

(defmethod input-delete-forward ((input text-input))
  "Delete character at cursor (delete key)."
  (let* ((value (input-value input))
         (pos (input-cursor-pos input)))
    (when (< pos (length value))
      (setf (input-value input)
            (concatenate 'string
                         (subseq value 0 pos)
                         (subseq value (1+ pos))))
      (setf (panel-dirty-p input) t))))

(defmethod input-move-cursor ((input text-input) direction)
  "Move cursor by DIRECTION (-1 left, +1 right)."
  (let* ((value (input-value input))
         (pos (input-cursor-pos input))
         (new-pos (+ pos direction)))
    (when (and (>= new-pos 0) (<= new-pos (length value)))
      (setf (input-cursor-pos input) new-pos)
      (setf (panel-dirty-p input) t))))

(defmethod input-move-to-start ((input text-input))
  "Move cursor to start."
  (setf (input-cursor-pos input) 0)
  (setf (input-scroll-offset input) 0)
  (setf (panel-dirty-p input) t))

(defmethod input-move-to-end ((input text-input))
  "Move cursor to end."
  (setf (input-cursor-pos input) (length (input-value input)))
  (setf (panel-dirty-p input) t))

(defmethod input-clear ((input text-input))
  "Clear all text."
  (setf (input-value input) "")
  (setf (input-cursor-pos input) 0)
  (setf (input-scroll-offset input) 0)
  (setf (panel-dirty-p input) t))

(defmethod input-handle-key ((input text-input) key-event)
  "Handle key event. Returns T if handled."
  (let ((char (key-event-char key-event))
        (code (key-event-code key-event))
        (ctrl (key-event-ctrl-p key-event)))
    (cond
      ;; Printable character
      ((and char (graphic-char-p char) (not ctrl))
       (input-insert-char input char)
       t)
      ;; Backspace
      ((eql code +key-backspace+)
       (input-delete-char input)
       t)
      ;; Delete
      ((eql code +key-delete+)
       (input-delete-forward input)
       t)
      ;; Left arrow
      ((eql code +key-left+)
       (input-move-cursor input -1)
       t)
      ;; Right arrow
      ((eql code +key-right+)
       (input-move-cursor input 1)
       t)
      ;; Home
      ((eql code +key-home+)
       (input-move-to-start input)
       t)
      ;; End
      ((eql code +key-end+)
       (input-move-to-end input)
       t)
      ;; Ctrl-A (start)
      ((and ctrl (eql char #\a))
       (input-move-to-start input)
       t)
      ;; Ctrl-E (end)
      ((and ctrl (eql char #\e))
       (input-move-to-end input)
       t)
      ;; Ctrl-K (kill to end)
      ((and ctrl (eql char #\k))
       (setf (input-value input) (subseq (input-value input) 0 (input-cursor-pos input)))
       (setf (panel-dirty-p input) t)
       t)
      ;; Ctrl-U (kill to start)
      ((and ctrl (eql char #\u))
       (setf (input-value input) (subseq (input-value input) (input-cursor-pos input)))
       (setf (input-cursor-pos input) 0)
       (setf (panel-dirty-p input) t)
       t)
      (t nil))))

(defmethod panel-render ((input text-input))
  "Render the text input field."
  (when (panel-visible-p input)
    (let* ((x (panel-x input))
           (y (panel-y input))
           (w (panel-width input))
           (value (input-value input))
           (pos (input-cursor-pos input))
           (placeholder (input-placeholder input))
           (password-p (input-password-p input))
           (active (panel-active-p input)))
      ;; Determine display text
      (let* ((display-text (cond
                             ((and (zerop (length value)) (not active))
                              placeholder)
                             (password-p
                              (make-string (length value) :initial-element #\*))
                             (t value)))
             ;; Calculate scroll offset to keep cursor visible
             (scroll (input-scroll-offset input))
             (visible-width w))
        ;; Adjust scroll if cursor is out of view
        (when (< pos scroll)
          (setf scroll pos))
        (when (>= pos (+ scroll visible-width))
          (setf scroll (1+ (- pos visible-width))))
        (setf (input-scroll-offset input) scroll)
        ;; Render
        (cursor-to y x)
        (when (and (zerop (length value)) (not active))
          (dim))
        (let* ((visible-text (if (> (length display-text) scroll)
                                 (subseq display-text scroll)
                                 ""))
               (padded (pad-string visible-text visible-width)))
          (princ padded *terminal-io*))
        (when (and (zerop (length value)) (not active))
          (reset))
        ;; Position cursor if active
        (when active
          (cursor-to y (+ x (- pos scroll))))
        (force-output *terminal-io*)))
    (setf (panel-dirty-p input) nil)))

;;; ============================================================
;;; Progress Bar Widget
;;; ============================================================

(defclass progress-bar (panel)
  ((progress :initarg :progress :accessor progress-value :initform 0
             :documentation "Progress value 0.0 to 1.0 (or nil for indeterminate)")
   (style :initarg :style :accessor progress-style :initform :bar
          :documentation "Style: :bar, :blocks, :dots")
   (fill-char :initarg :fill-char :accessor progress-fill-char :initform #\█)
   (empty-char :initarg :empty-char :accessor progress-empty-char :initform #\░)
   (show-percentage :initarg :show-percentage :accessor progress-show-percentage-p :initform t)
   (indeterminate-pos :initform 0 :accessor progress-indeterminate-pos
                      :documentation "Animation position for indeterminate mode"))
  (:default-initargs :height 1 :border nil)
  (:documentation "Progress bar with determinate and indeterminate modes."))

(defgeneric progress-set (bar value)
  (:documentation "Set progress value (0.0-1.0 or nil for indeterminate)."))

(defgeneric progress-tick (bar)
  (:documentation "Advance indeterminate animation by one step."))

(defmethod progress-set ((bar progress-bar) value)
  "Set progress to VALUE (0.0-1.0 or nil)."
  (setf (progress-value bar) (when value (max 0.0 (min 1.0 value))))
  (setf (panel-dirty-p bar) t))

(defmethod progress-tick ((bar progress-bar))
  "Advance indeterminate animation."
  (incf (progress-indeterminate-pos bar))
  (setf (panel-dirty-p bar) t))

(defmethod panel-render ((bar progress-bar))
  "Render the progress bar."
  (when (panel-visible-p bar)
    (let* ((x (panel-x bar))
           (y (panel-y bar))
           (w (panel-width bar))
           (progress (progress-value bar))
           (fill-char (progress-fill-char bar))
           (empty-char (progress-empty-char bar))
           (show-pct (progress-show-percentage-p bar)))
      (cursor-to y x)
      (if progress
          ;; Determinate mode
          (let* ((pct-str (if show-pct (format nil " ~3D%" (round (* progress 100))) ""))
                 (bar-width (- w (length pct-str)))
                 (filled (round (* progress bar-width))))
            (loop repeat filled do (princ fill-char *terminal-io*))
            (loop repeat (- bar-width filled) do (princ empty-char *terminal-io*))
            (when show-pct
              (princ pct-str *terminal-io*)))
          ;; Indeterminate mode - bouncing block
          (let* ((pos (progress-indeterminate-pos bar))
                 (block-width 3)
                 (travel-width (- w block-width))
                 (cycle (* 2 travel-width))
                 (phase (mod pos cycle))
                 (block-pos (if (< phase travel-width)
                                phase
                                (- cycle phase))))
            (loop repeat block-pos do (princ empty-char *terminal-io*))
            (loop repeat block-width do (princ fill-char *terminal-io*))
            (loop repeat (- travel-width block-pos) do (princ empty-char *terminal-io*))))
      (force-output *terminal-io*))
    (setf (panel-dirty-p bar) nil)))

;;; ============================================================
;;; Status Bar Widget
;;; ============================================================

(defclass status-bar (panel)
  ((sections :initarg :sections :accessor status-sections :initform nil
             :documentation "List of (text . width) pairs, width can be :flex")
   (separator :initarg :separator :accessor status-separator :initform " │ ")
   (align :initarg :align :accessor status-align :initform :left
          :documentation "Default text alignment: :left, :center, :right")
   (style :initarg :style :accessor status-style :initform nil
          :documentation "Text style for the status bar"))
  (:default-initargs :height 1 :border nil)
  (:documentation "Fixed-position status bar with multiple sections."))

(defgeneric status-set-section (bar index text)
  (:documentation "Set the text of section at INDEX."))

(defgeneric status-set-sections (bar &rest texts)
  (:documentation "Set all section texts at once."))

(defmethod status-set-section ((bar status-bar) index text)
  "Set section INDEX to TEXT."
  (let ((sections (status-sections bar)))
    (when (and sections (< index (length sections)))
      (setf (car (nth index sections)) text)
      (setf (panel-dirty-p bar) t))))

(defmethod status-set-sections ((bar status-bar) &rest texts)
  "Set all sections from TEXTS list."
  (let ((sections (status-sections bar)))
    (loop for text in texts
          for section in sections
          do (setf (car section) text))
    (setf (panel-dirty-p bar) t)))

(defun align-text (text width align)
  "Align TEXT within WIDTH according to ALIGN (:left, :center, :right)."
  (let ((len (length text)))
    (cond
      ((>= len width) (subseq text 0 width))
      ((eq align :right)
       (concatenate 'string (make-string (- width len) :initial-element #\Space) text))
      ((eq align :center)
       (let ((left-pad (floor (- width len) 2)))
         (concatenate 'string
                      (make-string left-pad :initial-element #\Space)
                      text
                      (make-string (- width len left-pad) :initial-element #\Space))))
      (t ; :left
       (concatenate 'string text (make-string (- width len) :initial-element #\Space))))))

(defmethod panel-render ((bar status-bar))
  "Render the status bar."
  (when (panel-visible-p bar)
    (let* ((x (panel-x bar))
           (y (panel-y bar))
           (w (panel-width bar))
           (sections (status-sections bar))
           (sep (status-separator bar))
           (sep-len (length sep))
           (style (status-style bar))
           (default-align (status-align bar)))
      (cursor-to y x)
      (when style (emit-style style *terminal-io*))
      ;; Calculate widths
      (let* ((fixed-width (loop for (text . width) in sections
                                when (numberp width) sum width))
             (flex-count (count-if (lambda (s) (eq (cdr s) :flex)) sections))
             (sep-total (* sep-len (max 0 (1- (length sections)))))
             (remaining (- w fixed-width sep-total))
             (flex-width (if (> flex-count 0) (floor remaining flex-count) 0)))
        ;; Render sections
        (loop for (text . width) in sections
              for first = t then nil
              do (unless first (princ sep *terminal-io*))
                 (let ((actual-width (if (eq width :flex) flex-width width)))
                   (princ (align-text (or text "") actual-width default-align) *terminal-io*))))
      (when style (reset))
      (force-output *terminal-io*))
    (setf (panel-dirty-p bar) nil)))

;;; ============================================================
;;; Layout System
;;; ============================================================

(defclass layout ()
  ((x :initarg :x :accessor layout-x :initform 1)
   (y :initarg :y :accessor layout-y :initform 1)
   (width :initarg :width :accessor layout-width :initform 80)
   (height :initarg :height :accessor layout-height :initform 24)
   (children :initarg :children :accessor layout-children :initform nil
             :documentation "List of child panels or layouts")
   (direction :initarg :direction :accessor layout-direction :initform :horizontal
              :documentation "Split direction: :horizontal or :vertical")
   (sizes :initarg :sizes :accessor layout-sizes :initform nil
          :documentation "List of sizes for children - numbers (fixed) or :flex")
   (gap :initarg :gap :accessor layout-gap :initform 0
        :documentation "Gap between children")
   (visible-p :initarg :visible :accessor layout-visible-p :initform t))
  (:documentation "Container that arranges children horizontally or vertically."))

(defgeneric layout-add-child (layout child &optional size)
  (:documentation "Add a child panel/layout with optional size."))

(defgeneric layout-remove-child (layout child)
  (:documentation "Remove a child from the layout."))

(defgeneric layout-resize (layout width height)
  (:documentation "Resize the layout and recalculate children."))

(defgeneric layout-recalculate (layout)
  (:documentation "Recalculate child positions and sizes."))

(defgeneric layout-render (layout)
  (:documentation "Render all children in the layout."))

(defmethod layout-add-child ((layout layout) child &optional (size :flex))
  "Add CHILD with SIZE (:flex or fixed number)."
  (setf (layout-children layout) (append (layout-children layout) (list child)))
  (setf (layout-sizes layout) (append (layout-sizes layout) (list size)))
  (layout-recalculate layout))

(defmethod layout-remove-child ((layout layout) child)
  "Remove CHILD from layout."
  (let ((pos (position child (layout-children layout))))
    (when pos
      (setf (layout-children layout) (remove child (layout-children layout)))
      (setf (layout-sizes layout) 
            (append (subseq (layout-sizes layout) 0 pos)
                    (subseq (layout-sizes layout) (1+ pos))))
      (layout-recalculate layout))))

(defmethod layout-resize ((layout layout) width height)
  "Resize layout to new dimensions."
  (setf (layout-width layout) width)
  (setf (layout-height layout) height)
  (layout-recalculate layout))

(defmethod layout-recalculate ((layout layout))
  "Recalculate positions and sizes of all children."
  (let* ((children (layout-children layout))
         (sizes (layout-sizes layout))
         (direction (layout-direction layout))
         (gap (layout-gap layout))
         (horizontal-p (eq direction :horizontal))
         (total-size (if horizontal-p 
                         (layout-width layout) 
                         (layout-height layout)))
         (cross-size (if horizontal-p 
                         (layout-height layout) 
                         (layout-width layout)))
         (num-children (length children))
         (total-gap (* gap (max 0 (1- num-children))))
         (available (- total-size total-gap))
         ;; Calculate fixed and flex sizes
         (fixed-total (loop for size in sizes
                            when (numberp size) sum size))
         (flex-count (count :flex sizes))
         (flex-size (if (> flex-count 0)
                        (floor (- available fixed-total) flex-count)
                        0))
         (current-pos (if horizontal-p 
                          (layout-x layout) 
                          (layout-y layout))))
    ;; Position each child
    (loop for child in children
          for size in sizes
          for actual-size = (if (eq size :flex) flex-size size)
          do (if horizontal-p
                 (progn
                   (setf (panel-x child) current-pos)
                   (setf (panel-y child) (layout-y layout))
                   (setf (panel-width child) actual-size)
                   (setf (panel-height child) cross-size))
                 (progn
                   (setf (panel-x child) (layout-x layout))
                   (setf (panel-y child) current-pos)
                   (setf (panel-width child) cross-size)
                   (setf (panel-height child) actual-size)))
             ;; Recursively recalculate nested layouts
             (when (typep child 'layout)
               (layout-recalculate child))
             (incf current-pos (+ actual-size gap)))))

(defmethod layout-render ((layout layout))
  "Render all visible children."
  (when (layout-visible-p layout)
    (dolist (child (layout-children layout))
      (cond
        ((typep child 'layout)
         (layout-render child))
        ((typep child 'panel)
         (when (panel-visible-p child)
           (panel-render child)))))))

;;; ============================================================
;;; Split Pane Widget
;;; ============================================================

(defclass split-pane (layout)
  ((first-child :initarg :first :accessor split-first :initform nil)
   (second-child :initarg :second :accessor split-second :initform nil)
   (split-pos :initarg :split-pos :accessor split-pos :initform 0.5
              :documentation "Split position as ratio (0.0-1.0) or fixed pixels")
   (min-size :initarg :min-size :accessor split-min-size :initform 5
             :documentation "Minimum size for either pane")
   (resizable-p :initarg :resizable :accessor split-resizable-p :initform t)
   (show-divider-p :initarg :show-divider :accessor split-show-divider-p :initform t)
   (divider-char :initarg :divider-char :accessor split-divider-char :initform nil))
  (:documentation "Two-pane split container with adjustable divider."))

(defgeneric split-set-children (split first second)
  (:documentation "Set both children of the split pane."))

(defgeneric split-adjust (split delta)
  (:documentation "Adjust split position by delta."))

(defmethod initialize-instance :after ((split split-pane) &key)
  "Initialize split pane with children."
  ;; Add children without triggering layout-recalculate yet
  (when (split-first split)
    (setf (layout-children split) (list (split-first split)))
    (setf (layout-sizes split) (list :flex)))
  (when (split-second split)
    (setf (layout-children split) (append (layout-children split) (list (split-second split))))
    (setf (layout-sizes split) (append (layout-sizes split) (list :flex))))
  ;; Now recalculate with proper split positioning
  (split-recalculate split))

(defmethod split-set-children ((split split-pane) first second)
  "Set both children."
  (setf (layout-children split) nil)
  (setf (layout-sizes split) nil)
  (setf (split-first split) first)
  (setf (split-second split) second)
  (when first (layout-add-child split first))
  (when second (layout-add-child split second))
  (split-recalculate split))

(defun split-recalculate (split)
  "Recalculate split pane sizes based on split-pos."
  (let* ((direction (layout-direction split))
         (horizontal-p (eq direction :horizontal))
         (total (if horizontal-p 
                    (layout-width split) 
                    (layout-height split)))
         (divider-size (if (split-show-divider-p split) 1 0))
         (available (- total divider-size))
         (pos (split-pos split))
         (first-size (if (floatp pos)
                         (floor (* available pos))
                         pos))
         (second-size (- available first-size))
         (min-size (split-min-size split)))
    ;; Enforce minimum sizes
    (when (< first-size min-size)
      (setf first-size min-size)
      (setf second-size (- available first-size)))
    (when (< second-size min-size)
      (setf second-size min-size)
      (setf first-size (- available second-size)))
    ;; Update sizes list
    (setf (layout-sizes split) (list first-size second-size))
    (setf (layout-gap split) divider-size)
    (layout-recalculate split)))

(defmethod split-adjust ((split split-pane) delta)
  "Adjust split position by DELTA pixels."
  (when (split-resizable-p split)
    (let* ((direction (layout-direction split))
           (horizontal-p (eq direction :horizontal))
           (total (if horizontal-p 
                      (layout-width split) 
                      (layout-height split)))
           (divider-size (if (split-show-divider-p split) 1 0))
           (available (- total divider-size))
           (current-first (first (layout-sizes split)))
           (new-first (+ current-first delta))
           (min-size (split-min-size split)))
      ;; Clamp to valid range
      (setf new-first (max min-size (min new-first (- available min-size))))
      ;; Update split-pos as float ratio
      (setf (split-pos split) (float (/ new-first available)))
      (split-recalculate split))))

(defmethod layout-resize :after ((split split-pane) width height)
  "Recalculate split after resize."
  (declare (ignore width height))
  (split-recalculate split))

(defmethod layout-render :after ((split split-pane))
  "Render divider line if enabled."
  (when (and (layout-visible-p split) (split-show-divider-p split))
    (let* ((direction (layout-direction split))
           (horizontal-p (eq direction :horizontal))
           (first-size (first (layout-sizes split)))
           (divider-char (or (split-divider-char split)
                             (if horizontal-p #\│ #\─))))
      (if horizontal-p
          ;; Vertical divider line
          (let ((x (+ (layout-x split) first-size)))
            (loop for row from (layout-y split) 
                  below (+ (layout-y split) (layout-height split))
                  do (cursor-to row x)
                     (princ divider-char *terminal-io*)))
          ;; Horizontal divider line
          (let ((y (+ (layout-y split) first-size)))
            (cursor-to y (layout-x split))
            (loop repeat (layout-width split)
                  do (princ divider-char *terminal-io*))))
      (force-output *terminal-io*))))

;;; ============================================================
;;; Modal Dialog Widget
;;; ============================================================

(defclass modal-dialog (panel)
  ((message :initarg :message :accessor dialog-message :initform ""
            :documentation "Main message text")
   (buttons :initarg :buttons :accessor dialog-buttons :initform '("OK")
            :documentation "List of button labels")
   (selected-button :initarg :selected :accessor dialog-selected-button :initform 0
                    :documentation "Currently selected button index")
   (result :accessor dialog-result :initform nil
           :documentation "Result after dialog closes")
   (closed-p :accessor dialog-closed-p :initform nil
             :documentation "Whether dialog has been closed")
   (button-style :initarg :button-style :accessor dialog-button-style :initform nil)
   (selected-style :initarg :selected-style :accessor dialog-selected-style :initform nil))
  (:default-initargs :border t)
  (:documentation "Modal dialog with message and buttons."))

(defgeneric dialog-select-next (dialog)
  (:documentation "Select next button."))

(defgeneric dialog-select-prev (dialog)
  (:documentation "Select previous button."))

(defgeneric dialog-confirm (dialog)
  (:documentation "Confirm selection and close dialog."))

(defgeneric dialog-cancel (dialog)
  (:documentation "Cancel dialog."))

(defgeneric dialog-handle-key (dialog key-event)
  (:documentation "Handle key event in dialog."))

(defmethod dialog-select-next ((dialog modal-dialog))
  "Select next button."
  (let ((num-buttons (length (dialog-buttons dialog))))
    (setf (dialog-selected-button dialog)
          (mod (1+ (dialog-selected-button dialog)) num-buttons))
    (setf (panel-dirty-p dialog) t)))

(defmethod dialog-select-prev ((dialog modal-dialog))
  "Select previous button."
  (let ((num-buttons (length (dialog-buttons dialog))))
    (setf (dialog-selected-button dialog)
          (mod (1- (dialog-selected-button dialog)) num-buttons))
    (setf (panel-dirty-p dialog) t)))

(defmethod dialog-confirm ((dialog modal-dialog))
  "Confirm and close with selected button."
  (setf (dialog-result dialog) (dialog-selected-button dialog))
  (setf (dialog-closed-p dialog) t))

(defmethod dialog-cancel ((dialog modal-dialog))
  "Cancel dialog."
  (setf (dialog-result dialog) nil)
  (setf (dialog-closed-p dialog) t))

(defmethod dialog-handle-key ((dialog modal-dialog) key-event)
  "Handle key event. Returns T if handled."
  (let ((code (key-event-code key-event))
        (char (key-event-char key-event)))
    (cond
      ((or (eql code +key-left+) (eql char #\h))
       (dialog-select-prev dialog) t)
      ((or (eql code +key-right+) (eql char #\l))
       (dialog-select-next dialog) t)
      ((eql code +key-tab+)
       (dialog-select-next dialog) t)
      ((or (eql code +key-enter+) (eql char #\Space))
       (dialog-confirm dialog) t)
      ((or (eql code +key-escape+) (eql char #\q))
       (dialog-cancel dialog) t)
      (t nil))))

(defun wrap-text (text width)
  "Wrap TEXT to fit within WIDTH, returning list of lines."
  (let ((lines nil)
        (current-line "")
        (words (split-string text)))
    (dolist (word words)
      (if (> (+ (length current-line) 1 (length word)) width)
          (progn
            (when (> (length current-line) 0)
              (push current-line lines))
            (setf current-line word))
          (setf current-line
                (if (zerop (length current-line))
                    word
                    (concatenate 'string current-line " " word)))))
    (when (> (length current-line) 0)
      (push current-line lines))
    (nreverse lines)))

(defun split-string (string &optional (delimiter #\Space))
  "Split STRING by DELIMITER."
  (let ((result nil)
        (start 0))
    (loop for i from 0 to (length string)
          do (when (or (= i (length string))
                       (char= (char string i) delimiter))
               (when (> i start)
                 (push (subseq string start i) result))
               (setf start (1+ i))))
    (nreverse result)))

(defmethod panel-render ((dialog modal-dialog))
  "Render the modal dialog."
  (when (panel-visible-p dialog)
    (let* ((x (panel-x dialog))
           (y (panel-y dialog))
           (w (panel-width dialog))
           (h (panel-height dialog))
           (title (panel-title dialog))
           (message (dialog-message dialog))
           (buttons (dialog-buttons dialog))
           (selected (dialog-selected-button dialog))
           (content-w (- w 4))  ; Account for border and padding
           (lines (wrap-text message content-w)))
      ;; Draw border
      (when (panel-border-p dialog)
        (draw-box x y w h)
        ;; Draw title if present
        (when title
          (cursor-to y (+ x 2))
          (format *terminal-io* " ~A " title)))
      ;; Draw message lines
      (let ((msg-y (+ y 2)))
        (dolist (line lines)
          (cursor-to msg-y (+ x 2))
          (princ (pad-string line content-w) *terminal-io*)
          (incf msg-y)))
      ;; Draw buttons at bottom
      (let* ((button-y (- (+ y h) 2))
             (total-btn-width (+ (reduce #'+ buttons :key #'length)
                                 (* 4 (length buttons))  ; brackets and spaces
                                 (* 2 (1- (length buttons)))))  ; gaps
             (btn-x (+ x (floor (- w total-btn-width) 2))))
        (cursor-to button-y btn-x)
        (loop for btn in buttons
              for i from 0
              do (when (> i 0) (princ "  " *terminal-io*))
                 (if (= i selected)
                     (progn
                       (reverse-video)
                       (format *terminal-io* "[ ~A ]" btn)
                       (reset))
                     (format *terminal-io* "[ ~A ]" btn))))
      (force-output *terminal-io*))
    (setf (panel-dirty-p dialog) nil)))

(defun draw-box (x y width height &optional (stream *terminal-io*))
  "Draw a box at position (x, y) with given dimensions."
  (let ((x2 (+ x width -1))
        (y2 (+ y height -1)))
    ;; Top border
    (cursor-to y x stream)
    (write-char #\┌ stream)
    (loop repeat (- width 2) do (write-char #\─ stream))
    (write-char #\┐ stream)
    ;; Side borders
    (loop for row from (1+ y) below y2
          do (cursor-to row x stream)
             (write-char #\│ stream)
             (cursor-to row x2 stream)
             (write-char #\│ stream))
    ;; Bottom border
    (cursor-to y2 x stream)
    (write-char #\└ stream)
    (loop repeat (- width 2) do (write-char #\─ stream))
    (write-char #\┘ stream)))

;;; ============================================================
;;; Input Dialog (extends Modal)
;;; ============================================================

(defclass input-dialog (modal-dialog)
  ((input :initarg :input :accessor dialog-input :initform nil
          :documentation "Text input widget")
   (prompt :initarg :prompt :accessor dialog-prompt :initform "Enter value:"
           :documentation "Prompt text above input"))
  (:default-initargs :buttons '("OK" "Cancel"))
  (:documentation "Modal dialog with text input field."))

(defmethod initialize-instance :after ((dialog input-dialog) &key)
  "Create the text input widget."
  (let* ((w (panel-width dialog))
         (input-w (- w 6)))
    (setf (dialog-input dialog)
          (make-instance 'text-input
                         :width input-w
                         :x (+ (panel-x dialog) 3)
                         :y (+ (panel-y dialog) 4)
                         :active t))))

(defmethod dialog-handle-key ((dialog input-dialog) key-event)
  "Handle key event for input dialog."
  (let ((code (key-event-code key-event))
        (input (dialog-input dialog)))
    (cond
      ((eql code +key-enter+)
       (dialog-confirm dialog) t)
      ((eql code +key-escape+)
       (dialog-cancel dialog) t)
      ((eql code +key-tab+)
       (dialog-select-next dialog) t)
      (t
       (input-handle-key input key-event)))))

(defmethod dialog-confirm :before ((dialog input-dialog))
  "Store input value as result."
  (when (zerop (dialog-selected-button dialog))
    (setf (dialog-result dialog) (input-value (dialog-input dialog)))))

(defmethod panel-render ((dialog input-dialog))
  "Render input dialog."
  (when (panel-visible-p dialog)
    (let* ((x (panel-x dialog))
           (y (panel-y dialog))
           (w (panel-width dialog))
           (h (panel-height dialog))
           (title (panel-title dialog))
           (prompt (dialog-prompt dialog))
           (buttons (dialog-buttons dialog))
           (selected (dialog-selected-button dialog))
           (input (dialog-input dialog)))
      ;; Draw border
      (draw-box x y w h)
      ;; Draw title
      (when title
        (cursor-to y (+ x 2))
        (format *terminal-io* " ~A " title))
      ;; Draw prompt
      (cursor-to (+ y 2) (+ x 2))
      (princ prompt *terminal-io*)
      ;; Update input position and render
      (setf (panel-x input) (+ x 3)
            (panel-y input) (+ y 3)
            (panel-width input) (- w 6))
      (panel-render input)
      ;; Draw buttons
      (let* ((button-y (- (+ y h) 2))
             (total-btn-width (+ (reduce #'+ buttons :key #'length)
                                 (* 4 (length buttons))
                                 (* 2 (1- (length buttons)))))
             (btn-x (+ x (floor (- w total-btn-width) 2))))
        (cursor-to button-y btn-x)
        (loop for btn in buttons
              for i from 0
              do (when (> i 0) (princ "  " *terminal-io*))
                 (if (= i selected)
                     (progn (reverse-video)
                            (format *terminal-io* "[ ~A ]" btn)
                            (reset))
                     (format *terminal-io* "[ ~A ]" btn))))
      (force-output *terminal-io*))
    (setf (panel-dirty-p dialog) nil)))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defun make-alert (message &key (title "Alert") (width 40) (height 8))
  "Create an alert dialog centered on screen."
  (let* ((term-size (terminal-size))
         (term-w (first term-size))
         (term-h (second term-size))
         (x (1+ (floor (- term-w width) 2)))
         (y (1+ (floor (- term-h height) 2))))
    (make-instance 'modal-dialog
                   :x x :y y :width width :height height
                   :title title :message message
                   :buttons '("OK"))))

(defun make-confirm (message &key (title "Confirm") (width 40) (height 8))
  "Create a confirmation dialog centered on screen."
  (let* ((term-size (terminal-size))
         (term-w (first term-size))
         (term-h (second term-size))
         (x (1+ (floor (- term-w width) 2)))
         (y (1+ (floor (- term-h height) 2))))
    (make-instance 'modal-dialog
                   :x x :y y :width width :height height
                   :title title :message message
                   :buttons '("Yes" "No"))))

(defun make-input-prompt (prompt &key (title "Input") (width 50) (height 8))
  "Create an input dialog centered on screen."
  (let* ((term-size (terminal-size))
         (term-w (first term-size))
         (term-h (second term-size))
         (x (1+ (floor (- term-w width) 2)))
         (y (1+ (floor (- term-h height) 2))))
    (make-instance 'input-dialog
                   :x x :y y :width width :height height
                   :title title :prompt prompt)))

;;; ============================================================
;;; Menu Item
;;; ============================================================

(defclass menu-item ()
  ((label :initarg :label :accessor menu-item-label :initform ""
          :documentation "Display text for menu item")
   (value :initarg :value :accessor menu-item-value :initform nil
          :documentation "Value returned when selected")
   (shortcut :initarg :shortcut :accessor menu-item-shortcut :initform nil
             :documentation "Keyboard shortcut character")
   (enabled-p :initarg :enabled :accessor menu-item-enabled-p :initform t
              :documentation "Whether item is selectable")
   (separator-p :initarg :separator :accessor menu-item-separator-p :initform nil
                :documentation "If true, this is a separator line"))
  (:documentation "A single menu item."))

(defun make-menu-item (label &key value shortcut (enabled t))
  "Create a menu item."
  (make-instance 'menu-item
                 :label label :value (or value label)
                 :shortcut shortcut :enabled enabled))

(defun make-separator ()
  "Create a menu separator."
  (make-instance 'menu-item :separator t))

;;; ============================================================
;;; Menu Widget
;;; ============================================================

(defclass menu (panel)
  ((items :initarg :items :accessor menu-items :initform nil
          :documentation "List of menu-item objects")
   (selected-index :initarg :selected :accessor menu-selected-index :initform 0
                   :documentation "Currently highlighted item index")
   (result :accessor menu-result :initform nil
           :documentation "Selected item value after close")
   (closed-p :accessor menu-closed-p :initform nil
             :documentation "Whether menu has been closed"))
  (:default-initargs :border t)
  (:documentation "Popup menu with keyboard navigation."))

(defgeneric menu-select-next (menu)
  (:documentation "Move selection to next enabled item."))

(defgeneric menu-select-prev (menu)
  (:documentation "Move selection to previous enabled item."))

(defgeneric menu-confirm (menu)
  (:documentation "Confirm current selection."))

(defgeneric menu-cancel (menu)
  (:documentation "Cancel menu."))

(defgeneric menu-handle-key (menu key-event)
  (:documentation "Handle key event in menu."))

(defun menu-selectable-p (item)
  "Return T if item can be selected."
  (and (not (menu-item-separator-p item))
       (menu-item-enabled-p item)))

(defmethod menu-select-next ((menu menu))
  "Move to next selectable item."
  (let* ((items (menu-items menu))
         (n (length items))
         (start (menu-selected-index menu)))
    (loop for i from 1 below n
          for idx = (mod (+ start i) n)
          for item = (nth idx items)
          when (menu-selectable-p item)
            do (setf (menu-selected-index menu) idx)
               (setf (panel-dirty-p menu) t)
               (return))))

(defmethod menu-select-prev ((menu menu))
  "Move to previous selectable item."
  (let* ((items (menu-items menu))
         (n (length items))
         (start (menu-selected-index menu)))
    (loop for i from 1 below n
          for idx = (mod (- start i) n)
          for item = (nth idx items)
          when (menu-selectable-p item)
            do (setf (menu-selected-index menu) idx)
               (setf (panel-dirty-p menu) t)
               (return))))

(defmethod menu-confirm ((menu menu))
  "Confirm selection."
  (let* ((idx (menu-selected-index menu))
         (item (nth idx (menu-items menu))))
    (when (and item (menu-selectable-p item))
      (setf (menu-result menu) (menu-item-value item))
      (setf (menu-closed-p menu) t))))

(defmethod menu-cancel ((menu menu))
  "Cancel menu."
  (setf (menu-result menu) nil)
  (setf (menu-closed-p menu) t))

(defmethod menu-handle-key ((menu menu) key-event)
  "Handle key event. Returns T if handled."
  (let ((code (key-event-code key-event))
        (char (key-event-char key-event)))
    ;; Check shortcuts first
    (when char
      (let ((items (menu-items menu)))
        (loop for item in items
              for i from 0
              when (and (menu-selectable-p item)
                        (menu-item-shortcut item)
                        (char-equal char (menu-item-shortcut item)))
                do (setf (menu-selected-index menu) i)
                   (menu-confirm menu)
                   (return-from menu-handle-key t))))
    ;; Navigation
    (cond
      ((or (eql code +key-up+) (eql char #\k))
       (menu-select-prev menu) t)
      ((or (eql code +key-down+) (eql char #\j))
       (menu-select-next menu) t)
      ((or (eql code +key-enter+) (eql char #\Space))
       (menu-confirm menu) t)
      ((or (eql code +key-escape+) (eql char #\q))
       (menu-cancel menu) t)
      ((eql code +key-home+)
       (setf (menu-selected-index menu) 0)
       (unless (menu-selectable-p (nth 0 (menu-items menu)))
         (menu-select-next menu))
       t)
      ((eql code +key-end+)
       (setf (menu-selected-index menu) (1- (length (menu-items menu))))
       (unless (menu-selectable-p (nth (menu-selected-index menu) (menu-items menu)))
         (menu-select-prev menu))
       t)
      (t nil))))

(defmethod panel-render ((menu menu))
  "Render the menu."
  (when (panel-visible-p menu)
    (let* ((x (panel-x menu))
           (y (panel-y menu))
           (w (panel-width menu))
           (h (panel-height menu))
           (title (panel-title menu))
           (items (menu-items menu))
           (selected (menu-selected-index menu)))
      ;; Draw border
      (draw-box x y w h)
      ;; Draw title
      (when title
        (cursor-to y (+ x 2))
        (format *terminal-io* " ~A " title))
      ;; Draw items
      (loop for item in items
            for i from 0
            for row = (+ y 1 i)
            while (< row (+ y h -1))
            do (cursor-to row (1+ x))
               (cond
                 ((menu-item-separator-p item)
                  ;; Separator line
                  (write-char #\├ *terminal-io*)
                  (loop repeat (- w 2) do (write-char #\─ *terminal-io*))
                  (write-char #\┤ *terminal-io*))
                 (t
                  ;; Regular item
                  (let* ((label (menu-item-label item))
                         (shortcut (menu-item-shortcut item))
                         (enabled (menu-item-enabled-p item))
                         (is-selected (= i selected))
                         (display-text (if shortcut
                                           (format nil "~A (~A)" label shortcut)
                                           label))
                         (padded (pad-string display-text (- w 2))))
                    (when is-selected
                      (reverse-video))
                    (unless enabled
                      (dim))
                    (princ padded *terminal-io*)
                    (when (or is-selected (not enabled))
                      (reset))))))
      (force-output *terminal-io*))
    (setf (panel-dirty-p menu) nil)))

(defun compute-menu-dimensions (items &optional title)
  "Compute width and height needed for menu items."
  (let* ((max-label (reduce #'max items
                            :key (lambda (item)
                                   (if (menu-item-separator-p item)
                                       0
                                       (+ (length (menu-item-label item))
                                          (if (menu-item-shortcut item) 4 0))))
                            :initial-value 0))
         (title-len (if title (+ (length title) 4) 0))
         (width (+ (max max-label title-len) 4))
         (height (+ (length items) 2)))
    (values width height)))

(defun make-menu (items &key title x y)
  "Create a popup menu. If x/y not specified, centers on screen."
  (multiple-value-bind (w h) (compute-menu-dimensions items title)
    (let* ((term-size (terminal-size))
           (term-w (first term-size))
           (term-h (second term-size))
           (menu-x (or x (1+ (floor (- term-w w) 2))))
           (menu-y (or y (1+ (floor (- term-h h) 2)))))
      (make-instance 'menu
                     :x menu-x :y menu-y :width w :height h
                     :title title :items items))))

;;; ============================================================
;;; Menu Bar
;;; ============================================================

(defclass menu-bar (panel)
  ((menus :initarg :menus :accessor menu-bar-menus :initform nil
          :documentation "Alist of (label . menu-items)")
   (selected-index :initarg :selected :accessor menu-bar-selected-index :initform 0
                   :documentation "Currently selected menu")
   (open-menu :accessor menu-bar-open-menu :initform nil
              :documentation "Currently open dropdown menu")
   (active-p :initarg :active :accessor menu-bar-active-p :initform nil
             :documentation "Whether menu bar is active"))
  (:default-initargs :height 1)
  (:documentation "Horizontal menu bar with dropdown menus."))

(defgeneric menu-bar-select-next (bar)
  (:documentation "Select next menu."))

(defgeneric menu-bar-select-prev (bar)
  (:documentation "Select previous menu."))

(defgeneric menu-bar-open (bar)
  (:documentation "Open currently selected menu."))

(defgeneric menu-bar-close (bar)
  (:documentation "Close open menu."))

(defgeneric menu-bar-handle-key (bar key-event)
  (:documentation "Handle key event."))

(defmethod menu-bar-select-next ((bar menu-bar))
  "Select next menu."
  (let ((n (length (menu-bar-menus bar))))
    (when (> n 0)
      (setf (menu-bar-selected-index bar)
            (mod (1+ (menu-bar-selected-index bar)) n))
      (setf (panel-dirty-p bar) t)
      ;; If menu is open, switch to new menu
      (when (menu-bar-open-menu bar)
        (menu-bar-open bar)))))

(defmethod menu-bar-select-prev ((bar menu-bar))
  "Select previous menu."
  (let ((n (length (menu-bar-menus bar))))
    (when (> n 0)
      (setf (menu-bar-selected-index bar)
            (mod (1- (menu-bar-selected-index bar)) n))
      (setf (panel-dirty-p bar) t)
      (when (menu-bar-open-menu bar)
        (menu-bar-open bar)))))

(defmethod menu-bar-open ((bar menu-bar))
  "Open dropdown for selected menu."
  (let* ((idx (menu-bar-selected-index bar))
         (menus (menu-bar-menus bar))
         (entry (nth idx menus)))
    (when entry
      (let* ((label (car entry))
             (items (cdr entry))
             ;; Calculate x position
             (x (+ (panel-x bar)
                   (loop for i below idx
                         for e in menus
                         sum (+ (length (car e)) 3))))
             (y (+ (panel-y bar) 1)))
        (declare (ignore label))
        (setf (menu-bar-open-menu bar)
              (make-menu items :x x :y y))))))

(defmethod menu-bar-close ((bar menu-bar))
  "Close open menu."
  (setf (menu-bar-open-menu bar) nil)
  (setf (panel-dirty-p bar) t))

(defmethod menu-bar-handle-key ((bar menu-bar) key-event)
  "Handle key event. Returns T if handled, or selected value."
  (let ((open-menu (menu-bar-open-menu bar))
        (code (key-event-code key-event))
        (char (key-event-char key-event)))
    (cond
      ;; If menu is open, delegate to it
      (open-menu
       (cond
         ((or (eql code +key-left+) (eql char #\h))
          (menu-bar-select-prev bar) t)
         ((or (eql code +key-right+) (eql char #\l))
          (menu-bar-select-next bar) t)
         ((eql code +key-escape+)
          (menu-bar-close bar) t)
         (t
          (menu-handle-key open-menu key-event)
          (when (menu-closed-p open-menu)
            (let ((result (menu-result open-menu)))
              (menu-bar-close bar)
              (when result
                (return-from menu-bar-handle-key result))))
          t)))
      ;; Menu bar navigation
      ((or (eql code +key-left+) (eql char #\h))
       (menu-bar-select-prev bar) t)
      ((or (eql code +key-right+) (eql char #\l))
       (menu-bar-select-next bar) t)
      ((or (eql code +key-enter+) (eql code +key-down+) (eql char #\Space))
       (menu-bar-open bar) t)
      ((eql code +key-escape+)
       (setf (menu-bar-active-p bar) nil) t)
      (t nil))))

(defmethod panel-render ((bar menu-bar))
  "Render the menu bar."
  (when (panel-visible-p bar)
    (let* ((x (panel-x bar))
           (y (panel-y bar))
           (w (panel-width bar))
           (menus (menu-bar-menus bar))
           (selected (menu-bar-selected-index bar))
           (active (menu-bar-active-p bar)))
      ;; Clear line
      (cursor-to y x)
      (reverse-video)
      (loop repeat w do (write-char #\Space *terminal-io*))
      ;; Draw menu labels
      (cursor-to y x)
      (loop for entry in menus
            for i from 0
            for label = (car entry)
            do (when (and active (= i selected))
                 (reset)
                 (bold))
               (format *terminal-io* " ~A " label)
               (when (and active (= i selected))
                 (reset)
                 (reverse-video)))
      (reset)
      ;; Render open dropdown
      (when (menu-bar-open-menu bar)
        (panel-render (menu-bar-open-menu bar)))
      (force-output *terminal-io*))
    (setf (panel-dirty-p bar) nil)))

;;; ============================================================
;;; Table Column
;;; ============================================================

(defclass table-column ()
  ((header :initarg :header :accessor column-header :initform ""
           :documentation "Column header text")
   (key :initarg :key :accessor column-key :initform nil
        :documentation "Key function or slot name to extract value from row")
   (width :initarg :width :accessor column-width :initform nil
          :documentation "Fixed width, or nil for auto")
   (min-width :initarg :min-width :accessor column-min-width :initform 3
              :documentation "Minimum column width")
   (align :initarg :align :accessor column-align :initform :left
          :documentation "Alignment: :left, :right, or :center")
   (formatter :initarg :formatter :accessor column-formatter :initform nil
              :documentation "Function to format cell value for display"))
  (:documentation "Definition of a table column."))

(defun make-column (header key &key width min-width (align :left) formatter)
  "Create a table column definition."
  (make-instance 'table-column
                 :header header :key key
                 :width width :min-width (or min-width 3)
                 :align align :formatter formatter))

;;; ============================================================
;;; Table Widget
;;; ============================================================

(defclass table-widget (panel)
  ((columns :initarg :columns :accessor table-columns :initform nil
            :documentation "List of table-column objects")
   (rows :initarg :rows :accessor table-rows :initform nil
         :documentation "List of row data (plists, alists, or objects)")
   (selected-row :initarg :selected :accessor table-selected-row :initform 0
                 :documentation "Currently selected row index")
   (scroll-offset :initarg :scroll :accessor table-scroll-offset :initform 0
                  :documentation "First visible row index")
   (show-header-p :initarg :show-header :accessor table-show-header-p :initform t
                  :documentation "Whether to show column headers")
   (column-widths :accessor table-column-widths :initform nil
                  :documentation "Computed column widths")
   (sortable-p :initarg :sortable :accessor table-sortable-p :initform nil
               :documentation "Whether columns are sortable")
   (sort-column :accessor table-sort-column :initform nil
                :documentation "Index of column being sorted")
   (sort-ascending-p :accessor table-sort-ascending-p :initform t
                     :documentation "Sort direction"))
  (:default-initargs :border t)
  (:documentation "Table widget with columns, rows, and optional sorting."))

(defgeneric table-select-next (table)
  (:documentation "Select next row."))

(defgeneric table-select-prev (table)
  (:documentation "Select previous row."))

(defgeneric table-page-down (table)
  (:documentation "Move down one page."))

(defgeneric table-page-up (table)
  (:documentation "Move up one page."))

(defgeneric table-select-first (table)
  (:documentation "Select first row."))

(defgeneric table-select-last (table)
  (:documentation "Select last row."))

(defgeneric table-sort-by (table column-index)
  (:documentation "Sort table by column."))

(defgeneric table-handle-key (table key-event)
  (:documentation "Handle key event."))

(defgeneric table-selected-data (table)
  (:documentation "Get currently selected row data."))

(defun table-visible-rows (table)
  "Return number of visible rows."
  (let* ((h (panel-height table))
         (border (if (panel-border-p table) 2 0))
         (header (if (table-show-header-p table) 2 0)))  ; header + separator
    (max 1 (- h border header))))

(defun table-ensure-visible (table)
  "Ensure selected row is visible."
  (let ((selected (table-selected-row table))
        (offset (table-scroll-offset table))
        (visible (table-visible-rows table)))
    (cond
      ((< selected offset)
       (setf (table-scroll-offset table) selected))
      ((>= selected (+ offset visible))
       (setf (table-scroll-offset table) (- selected visible -1))))))

(defmethod table-select-next ((table table-widget))
  "Select next row."
  (let ((n (length (table-rows table))))
    (when (> n 0)
      (setf (table-selected-row table)
            (min (1- n) (1+ (table-selected-row table))))
      (table-ensure-visible table)
      (setf (panel-dirty-p table) t))))

(defmethod table-select-prev ((table table-widget))
  "Select previous row."
  (when (> (table-selected-row table) 0)
    (decf (table-selected-row table))
    (table-ensure-visible table)
    (setf (panel-dirty-p table) t)))

(defmethod table-page-down ((table table-widget))
  "Move down one page."
  (let* ((visible (table-visible-rows table))
         (n (length (table-rows table))))
    (setf (table-selected-row table)
          (min (1- n) (+ (table-selected-row table) visible)))
    (table-ensure-visible table)
    (setf (panel-dirty-p table) t)))

(defmethod table-page-up ((table table-widget))
  "Move up one page."
  (let ((visible (table-visible-rows table)))
    (setf (table-selected-row table)
          (max 0 (- (table-selected-row table) visible)))
    (table-ensure-visible table)
    (setf (panel-dirty-p table) t)))

(defmethod table-select-first ((table table-widget))
  "Select first row."
  (setf (table-selected-row table) 0)
  (table-ensure-visible table)
  (setf (panel-dirty-p table) t))

(defmethod table-select-last ((table table-widget))
  "Select last row."
  (let ((n (length (table-rows table))))
    (when (> n 0)
      (setf (table-selected-row table) (1- n))
      (table-ensure-visible table)
      (setf (panel-dirty-p table) t))))

(defmethod table-selected-data ((table table-widget))
  "Get currently selected row data."
  (let ((rows (table-rows table))
        (idx (table-selected-row table)))
    (when (and rows (< idx (length rows)))
      (nth idx rows))))

(defun get-cell-value (row column)
  "Extract cell value from row using column key."
  (let ((key (column-key column)))
    (cond
      ((null key) "")
      ((functionp key) (funcall key row))
      ((symbolp key)
       (cond
         ((listp row)
          (if (consp (car row))
              ;; Alist
              (cdr (assoc key row))
              ;; Plist
              (getf row key)))
         (t
          ;; Object - try slot
          (if (slot-boundp row key)
              (slot-value row key)
              ""))))
      (t ""))))

(defun format-cell-value (value column)
  "Format cell value for display."
  (let ((formatter (column-formatter column)))
    (if formatter
        (funcall formatter value)
        (if value (format nil "~A" value) ""))))

(defun compute-column-widths (table)
  "Compute column widths based on content."
  (let* ((columns (table-columns table))
         (rows (table-rows table))
         (w (panel-width table))
         (border (if (panel-border-p table) 2 0))
         (available (- w border (1- (length columns))))  ; space for separators
         (widths (make-list (length columns) :initial-element 0)))
    ;; Calculate max width for each column
    (loop for col in columns
          for i from 0
          do (let ((header-w (length (column-header col)))
                   (max-data-w 0))
               ;; Check data widths
               (dolist (row rows)
                 (let* ((val (get-cell-value row col))
                        (str (format-cell-value val col)))
                   (setf max-data-w (max max-data-w (length str)))))
               ;; Use fixed width if specified, otherwise auto
               (setf (nth i widths)
                     (or (column-width col)
                         (max (column-min-width col)
                              header-w
                              max-data-w)))))
    ;; Scale down if total exceeds available
    (let ((total (reduce #'+ widths)))
      (when (> total available)
        (let ((scale (/ available total)))
          (setf widths (mapcar (lambda (w) 
                                 (max 3 (floor (* w scale))))
                               widths)))))
    (setf (table-column-widths table) widths)))

(defun align-string (str width align)
  "Align string within width."
  (let ((len (length str)))
    (cond
      ((>= len width) (subseq str 0 width))
      ((eq align :right)
       (concatenate 'string (make-string (- width len) :initial-element #\Space) str))
      ((eq align :center)
       (let* ((pad (- width len))
              (left (floor pad 2))
              (right (- pad left)))
         (concatenate 'string
                      (make-string left :initial-element #\Space)
                      str
                      (make-string right :initial-element #\Space))))
      (t  ; :left
       (concatenate 'string str (make-string (- width len) :initial-element #\Space))))))

(defmethod table-sort-by ((table table-widget) column-index)
  "Sort table by column."
  (when (table-sortable-p table)
    (let* ((col (nth column-index (table-columns table)))
           (ascending (if (eql column-index (table-sort-column table))
                          (not (table-sort-ascending-p table))
                          t)))
      (setf (table-sort-column table) column-index)
      (setf (table-sort-ascending-p table) ascending)
      (setf (table-rows table)
            (sort (copy-list (table-rows table))
                  (lambda (a b)
                    (let ((va (get-cell-value a col))
                          (vb (get-cell-value b col)))
                      (if ascending
                          (string< (format nil "~A" va) (format nil "~A" vb))
                          (string> (format nil "~A" va) (format nil "~A" vb)))))))
      (setf (panel-dirty-p table) t))))

(defmethod table-handle-key ((table table-widget) key-event)
  "Handle key event. Returns T if handled."
  (let ((code (key-event-code key-event))
        (char (key-event-char key-event)))
    (cond
      ((or (eql code +key-up+) (eql char #\k))
       (table-select-prev table) t)
      ((or (eql code +key-down+) (eql char #\j))
       (table-select-next table) t)
      ((eql code +key-page-down+)
       (table-page-down table) t)
      ((eql code +key-page-up+)
       (table-page-up table) t)
      ((or (eql code +key-home+) (eql char #\g))
       (table-select-first table) t)
      ((or (eql code +key-end+) (eql char #\G))
       (table-select-last table) t)
      ;; Number keys for sorting columns 1-9
      ((and (table-sortable-p table)
            char
            (digit-char-p char)
            (> (digit-char-p char) 0)
            (<= (digit-char-p char) (length (table-columns table))))
       (table-sort-by table (1- (digit-char-p char))) t)
      (t nil))))

(defmethod panel-render ((table table-widget))
  "Render the table."
  (when (panel-visible-p table)
    (let* ((x (panel-x table))
           (y (panel-y table))
           (w (panel-width table))
           (h (panel-height table))
           (columns (table-columns table))
           (rows (table-rows table))
           (selected (table-selected-row table))
           (offset (table-scroll-offset table))
           (show-header (table-show-header-p table))
           (border-p (panel-border-p table)))
      ;; Compute column widths if needed
      (unless (table-column-widths table)
        (compute-column-widths table))
      (let ((widths (table-column-widths table))
            (content-x (if border-p (1+ x) x))
            (content-y (if border-p (1+ y) y))
            (content-w (if border-p (- w 2) w))
            (content-h (if border-p (- h 2) h)))
        ;; Draw border
        (when border-p
          (draw-box x y w h))
        ;; Draw header
        (when show-header
          (cursor-to content-y content-x)
          (bold)
          (loop for col in columns
                for width in widths
                for i from 0
                do (when (> i 0) (write-char #\│ *terminal-io*))
                   (let* ((header (column-header col))
                          (sort-indicator (if (and (table-sortable-p table)
                                                   (eql i (table-sort-column table)))
                                              (if (table-sort-ascending-p table) "▲" "▼")
                                              ""))
                          (text (concatenate 'string header sort-indicator)))
                     (princ (align-string text width (column-align col)) *terminal-io*)))
          (reset)
          ;; Header separator
          (cursor-to (1+ content-y) content-x)
          (loop for width in widths
                for i from 0
                do (when (> i 0) (write-char #\┼ *terminal-io*))
                   (loop repeat width do (write-char #\─ *terminal-io*))))
        ;; Draw rows
        (let ((data-y (+ content-y (if show-header 2 0)))
              (visible (table-visible-rows table)))
          (loop for row-idx from offset below (min (length rows) (+ offset visible))
                for row = (nth row-idx rows)
                for row-y = (+ data-y (- row-idx offset))
                do (cursor-to row-y content-x)
                   (when (= row-idx selected)
                     (reverse-video))
                   (loop for col in columns
                         for width in widths
                         for i from 0
                         do (when (> i 0) (write-char #\│ *terminal-io*))
                            (let* ((val (get-cell-value row col))
                                   (str (format-cell-value val col)))
                              (princ (align-string str width (column-align col)) *terminal-io*)))
                   (when (= row-idx selected)
                     (reset))))
        (force-output *terminal-io*)))
    (setf (panel-dirty-p table) nil)))

;;; ============================================================
;;; Form Field
;;; ============================================================

(defclass form-field ()
  ((label :initarg :label :accessor field-label :initform ""
          :documentation "Label displayed before field")
   (widget :initarg :widget :accessor field-widget :initform nil
           :documentation "The input widget (text-input, etc.)")
   (key :initarg :key :accessor field-key :initform nil
        :documentation "Key for retrieving value")
   (required-p :initarg :required :accessor field-required-p :initform nil
               :documentation "Whether field is required")
   (validator :initarg :validator :accessor field-validator :initform nil
              :documentation "Function to validate field value")
   (error-message :accessor field-error-message :initform nil
                  :documentation "Current validation error"))
  (:documentation "A single form field with label and widget."))

(defun make-field (label key &key (type :text) required validator width value password)
  "Create a form field with appropriate widget."
  (let ((widget (ecase type
                  (:text (make-instance 'text-input
                                        :width (or width 20)
                                        :value (or value "")
                                        :password password)))))
    (make-instance 'form-field
                   :label label :key key
                   :widget widget
                   :required required
                   :validator validator)))

(defun field-value (field)
  "Get the current value of a form field."
  (let ((widget (field-widget field)))
    (when widget
      (input-value widget))))

(defun (setf field-value) (value field)
  "Set the value of a form field."
  (let ((widget (field-widget field)))
    (when widget
      (setf (input-value widget) value))))

(defun field-validate (field)
  "Validate field, return T if valid, set error-message if not."
  (let ((value (field-value field))
        (required (field-required-p field))
        (validator (field-validator field)))
    (cond
      ((and required (or (null value) (string= value "")))
       (setf (field-error-message field) "Required")
       nil)
      ((and validator (not (funcall validator value)))
       (setf (field-error-message field) "Invalid")
       nil)
      (t
       (setf (field-error-message field) nil)
       t))))

;;; ============================================================
;;; Form Widget
;;; ============================================================

(defclass form-widget (panel)
  ((fields :initarg :fields :accessor form-fields :initform nil
           :documentation "List of form-field objects")
   (focused-index :initarg :focused :accessor form-focused-index :initform 0
                  :documentation "Currently focused field index")
   (label-width :initarg :label-width :accessor form-label-width :initform 15
                :documentation "Width for labels column")
   (submitted-p :accessor form-submitted-p :initform nil
                :documentation "Whether form was submitted")
   (cancelled-p :accessor form-cancelled-p :initform nil
                :documentation "Whether form was cancelled")
   (show-errors-p :initarg :show-errors :accessor form-show-errors-p :initform t
                  :documentation "Whether to show validation errors"))
  (:default-initargs :border t)
  (:documentation "Form with multiple input fields and tab navigation."))

(defgeneric form-focus-next (form)
  (:documentation "Focus next field."))

(defgeneric form-focus-prev (form)
  (:documentation "Focus previous field."))

(defgeneric form-submit (form)
  (:documentation "Submit form if valid."))

(defgeneric form-cancel (form)
  (:documentation "Cancel form."))

(defgeneric form-handle-key (form key-event)
  (:documentation "Handle key event."))

(defgeneric form-values (form)
  (:documentation "Get all form values as plist."))

(defgeneric form-validate (form)
  (:documentation "Validate all fields."))

(defmethod initialize-instance :after ((form form-widget) &key)
  "Initialize field positions."
  (form-update-field-positions form))

(defun form-update-field-positions (form)
  "Update widget positions based on form layout."
  (let* ((x (panel-x form))
         (y (panel-y form))
         (border (if (panel-border-p form) 1 0))
         (label-w (form-label-width form))
         (field-x (+ x border label-w 2)))
    (loop for field in (form-fields form)
          for i from 0
          for field-y = (+ y border i)
          for widget = (field-widget field)
          when widget
            do (setf (panel-x widget) field-x
                     (panel-y widget) field-y))))

(defmethod form-focus-next ((form form-widget))
  "Focus next field."
  (let ((n (length (form-fields form))))
    (when (> n 0)
      ;; Deactivate current
      (let ((current (nth (form-focused-index form) (form-fields form))))
        (when (field-widget current)
          (setf (panel-active-p (field-widget current)) nil)))
      ;; Move to next
      (setf (form-focused-index form)
            (mod (1+ (form-focused-index form)) n))
      ;; Activate new
      (let ((new-field (nth (form-focused-index form) (form-fields form))))
        (when (field-widget new-field)
          (setf (panel-active-p (field-widget new-field)) t)))
      (setf (panel-dirty-p form) t))))

(defmethod form-focus-prev ((form form-widget))
  "Focus previous field."
  (let ((n (length (form-fields form))))
    (when (> n 0)
      ;; Deactivate current
      (let ((current (nth (form-focused-index form) (form-fields form))))
        (when (field-widget current)
          (setf (panel-active-p (field-widget current)) nil)))
      ;; Move to prev
      (setf (form-focused-index form)
            (mod (1- (form-focused-index form)) n))
      ;; Activate new
      (let ((new-field (nth (form-focused-index form) (form-fields form))))
        (when (field-widget new-field)
          (setf (panel-active-p (field-widget new-field)) t)))
      (setf (panel-dirty-p form) t))))

(defmethod form-validate ((form form-widget))
  "Validate all fields. Returns T if all valid."
  (let ((all-valid t))
    (dolist (field (form-fields form))
      (unless (field-validate field)
        (setf all-valid nil)))
    all-valid))

(defmethod form-submit ((form form-widget))
  "Submit form if valid."
  (when (form-validate form)
    (setf (form-submitted-p form) t)))

(defmethod form-cancel ((form form-widget))
  "Cancel form."
  (setf (form-cancelled-p form) t))

(defmethod form-values ((form form-widget))
  "Get all form values as plist."
  (let ((result nil))
    (dolist (field (form-fields form))
      (let ((key (field-key field))
            (value (field-value field)))
        (when key
          (setf (getf result key) value))))
    result))

(defmethod form-handle-key ((form form-widget) key-event)
  "Handle key event. Returns T if handled."
  (let ((code (key-event-code key-event))
        (focused-field (nth (form-focused-index form) (form-fields form))))
    (cond
      ;; Tab/Shift-Tab for navigation
      ((eql code +key-tab+)
       (if (key-event-shift-p key-event)
           (form-focus-prev form)
           (form-focus-next form))
       t)
      ;; Up/Down for navigation
      ((eql code +key-up+)
       (form-focus-prev form) t)
      ((eql code +key-down+)
       (form-focus-next form) t)
      ;; Enter to submit (when on last field or with Ctrl)
      ((eql code +key-enter+)
       (if (or (key-event-ctrl-p key-event)
               (= (form-focused-index form) (1- (length (form-fields form)))))
           (progn (form-submit form) t)
           (progn (form-focus-next form) t)))
      ;; Escape to cancel
      ((eql code +key-escape+)
       (form-cancel form) t)
      ;; Delegate to focused widget
      (focused-field
       (let ((widget (field-widget focused-field)))
         (when widget
           (input-handle-key widget key-event))))
      (t nil))))

(defmethod panel-render ((form form-widget))
  "Render the form."
  (when (panel-visible-p form)
    (let* ((x (panel-x form))
           (y (panel-y form))
           (w (panel-width form))
           (h (panel-height form))
           (title (panel-title form))
           (fields (form-fields form))
           (focused (form-focused-index form))
           (label-w (form-label-width form))
           (border-p (panel-border-p form))
           (show-errors (form-show-errors-p form)))
      ;; Draw border
      (when border-p
        (draw-box x y w h)
        (when title
          (cursor-to y (+ x 2))
          (format *terminal-io* " ~A " title)))
      ;; Update field positions
      (form-update-field-positions form)
      ;; Draw fields
      (let ((content-x (if border-p (1+ x) x))
            (content-y (if border-p (1+ y) y)))
        (loop for field in fields
              for i from 0
              for row = (+ content-y i)
              for is-focused = (= i focused)
              do ;; Draw label
                 (cursor-to row content-x)
                 (when is-focused (bold))
                 (let* ((label (field-label field))
                        (required (if (field-required-p field) "*" ""))
                        (full-label (format nil "~A~A:" label required)))
                   (princ (align-string full-label label-w :right) *terminal-io*))
                 (when is-focused (reset))
                 (write-char #\Space *terminal-io*)
                 ;; Render widget
                 (let ((widget (field-widget field)))
                   (when widget
                     (setf (panel-active-p widget) is-focused)
                     (panel-render widget)))
                 ;; Show error if any
                 (when (and show-errors (field-error-message field))
                   (write-char #\Space *terminal-io*)
                   (fg-color (make-named-color :red))
                   (princ (field-error-message field) *terminal-io*)
                   (reset))))
      (force-output *terminal-io*))
    (setf (panel-dirty-p form) nil)))

(defun make-form (fields &key title (label-width 15) x y width height)
  "Create a form with the given fields."
  (let* ((term-size (terminal-size))
         (term-w (first term-size))
         (term-h (second term-size))
         (form-h (or height (+ (length fields) 2)))
         (form-w (or width 60))
         (form-x (or x (1+ (floor (- term-w form-w) 2))))
         (form-y (or y (1+ (floor (- term-h form-h) 2)))))
    (make-instance 'form-widget
                   :fields fields
                   :title title
                   :label-width label-width
                   :x form-x :y form-y
                   :width form-w :height form-h)))

;;; ============================================================
;;; Tree Node
;;; ============================================================

(defclass tree-node ()
  ((label :initarg :label :accessor node-label :initform ""
          :documentation "Display text for node")
   (value :initarg :value :accessor node-value :initform nil
          :documentation "Associated value/data")
   (children :initarg :children :accessor node-children :initform nil
             :documentation "List of child tree-node objects")
   (expanded-p :initarg :expanded :accessor node-expanded-p :initform nil
               :documentation "Whether children are visible")
   (parent :initarg :parent :accessor node-parent :initform nil
           :documentation "Parent node reference"))
  (:documentation "A node in a tree structure."))

(defun make-node (label &key value children expanded)
  "Create a tree node."
  (let ((node (make-instance 'tree-node
                             :label label :value value
                             :children children :expanded expanded)))
    ;; Set parent references
    (dolist (child children)
      (setf (node-parent child) node))
    node))

(defun node-leaf-p (node)
  "Return T if node has no children."
  (null (node-children node)))

(defun node-depth (node)
  "Return depth of node (0 for root)."
  (if (node-parent node)
      (1+ (node-depth (node-parent node)))
      0))

;;; ============================================================
;;; Tree View Widget
;;; ============================================================

(defclass tree-view (panel)
  ((root :initarg :root :accessor tree-root :initform nil
         :documentation "Root tree-node (may be hidden)")
   (show-root-p :initarg :show-root :accessor tree-show-root-p :initform t
                :documentation "Whether to display root node")
   (selected-node :accessor tree-selected-node :initform nil
                  :documentation "Currently selected node")
   (scroll-offset :initarg :scroll :accessor tree-scroll-offset :initform 0
                  :documentation "First visible row index")
   (indent-size :initarg :indent :accessor tree-indent-size :initform 2
                :documentation "Spaces per indent level")
   (visible-nodes :accessor tree-visible-nodes :initform nil
                  :documentation "Cached list of visible nodes"))
  (:default-initargs :border t)
  (:documentation "Tree view with expandable/collapsible nodes."))

(defgeneric tree-select-next (tree)
  (:documentation "Select next visible node."))

(defgeneric tree-select-prev (tree)
  (:documentation "Select previous visible node."))

(defgeneric tree-toggle-expand (tree)
  (:documentation "Toggle expansion of selected node."))

(defgeneric tree-expand (tree)
  (:documentation "Expand selected node."))

(defgeneric tree-collapse (tree)
  (:documentation "Collapse selected node."))

(defgeneric tree-expand-all (tree)
  (:documentation "Expand all nodes."))

(defgeneric tree-collapse-all (tree)
  (:documentation "Collapse all nodes."))

(defgeneric tree-handle-key (tree key-event)
  (:documentation "Handle key event."))

(defgeneric tree-selected-value (tree)
  (:documentation "Get value of selected node."))

(defun tree-compute-visible (tree)
  "Compute list of visible nodes in display order."
  (let ((result nil))
    (labels ((collect (node depth)
               (push (cons node depth) result)
               (when (and (node-expanded-p node) (node-children node))
                 (dolist (child (node-children node))
                   (collect child (1+ depth))))))
      (when (tree-root tree)
        (if (tree-show-root-p tree)
            (collect (tree-root tree) 0)
            ;; Skip root, start with children
            (dolist (child (node-children (tree-root tree)))
              (collect child 0)))))
    (setf (tree-visible-nodes tree) (nreverse result))))

(defun tree-visible-count (tree)
  "Return number of visible rows."
  (let* ((h (panel-height tree))
         (border (if (panel-border-p tree) 2 0)))
    (max 1 (- h border))))

(defun tree-ensure-visible (tree)
  "Ensure selected node is visible."
  (let* ((nodes (tree-visible-nodes tree))
         (selected (tree-selected-node tree))
         (idx (position selected nodes :key #'car))
         (offset (tree-scroll-offset tree))
         (visible (tree-visible-count tree)))
    (when idx
      (cond
        ((< idx offset)
         (setf (tree-scroll-offset tree) idx))
        ((>= idx (+ offset visible))
         (setf (tree-scroll-offset tree) (- idx visible -1)))))))

(defmethod initialize-instance :after ((tree tree-view) &key)
  "Initialize tree view."
  (tree-compute-visible tree)
  (when (and (tree-visible-nodes tree) (null (tree-selected-node tree)))
    (setf (tree-selected-node tree) (caar (tree-visible-nodes tree)))))

(defmethod tree-select-next ((tree tree-view))
  "Select next visible node."
  (tree-compute-visible tree)
  (let* ((nodes (tree-visible-nodes tree))
         (selected (tree-selected-node tree))
         (idx (position selected nodes :key #'car)))
    (when (and idx (< idx (1- (length nodes))))
      (setf (tree-selected-node tree) (car (nth (1+ idx) nodes)))
      (tree-ensure-visible tree)
      (setf (panel-dirty-p tree) t))))

(defmethod tree-select-prev ((tree tree-view))
  "Select previous visible node."
  (tree-compute-visible tree)
  (let* ((nodes (tree-visible-nodes tree))
         (selected (tree-selected-node tree))
         (idx (position selected nodes :key #'car)))
    (when (and idx (> idx 0))
      (setf (tree-selected-node tree) (car (nth (1- idx) nodes)))
      (tree-ensure-visible tree)
      (setf (panel-dirty-p tree) t))))

(defmethod tree-toggle-expand ((tree tree-view))
  "Toggle expansion of selected node."
  (let ((node (tree-selected-node tree)))
    (when (and node (not (node-leaf-p node)))
      (setf (node-expanded-p node) (not (node-expanded-p node)))
      (tree-compute-visible tree)
      (setf (panel-dirty-p tree) t))))

(defmethod tree-expand ((tree tree-view))
  "Expand selected node."
  (let ((node (tree-selected-node tree)))
    (when (and node (not (node-leaf-p node)) (not (node-expanded-p node)))
      (setf (node-expanded-p node) t)
      (tree-compute-visible tree)
      (setf (panel-dirty-p tree) t))))

(defmethod tree-collapse ((tree tree-view))
  "Collapse selected node or go to parent."
  (let ((node (tree-selected-node tree)))
    (when node
      (cond
        ((and (not (node-leaf-p node)) (node-expanded-p node))
         ;; Collapse this node
         (setf (node-expanded-p node) nil)
         (tree-compute-visible tree)
         (setf (panel-dirty-p tree) t))
        ((node-parent node)
         ;; Go to parent
         (setf (tree-selected-node tree) (node-parent node))
         (tree-ensure-visible tree)
         (setf (panel-dirty-p tree) t))))))

(defun tree-expand-node-recursive (node)
  "Expand node and all descendants."
  (unless (node-leaf-p node)
    (setf (node-expanded-p node) t)
    (dolist (child (node-children node))
      (tree-expand-node-recursive child))))

(defun tree-collapse-node-recursive (node)
  "Collapse node and all descendants."
  (unless (node-leaf-p node)
    (setf (node-expanded-p node) nil)
    (dolist (child (node-children node))
      (tree-collapse-node-recursive child))))

(defmethod tree-expand-all ((tree tree-view))
  "Expand all nodes."
  (when (tree-root tree)
    (tree-expand-node-recursive (tree-root tree))
    (tree-compute-visible tree)
    (setf (panel-dirty-p tree) t)))

(defmethod tree-collapse-all ((tree tree-view))
  "Collapse all nodes."
  (when (tree-root tree)
    (tree-collapse-node-recursive (tree-root tree))
    (tree-compute-visible tree)
    (setf (panel-dirty-p tree) t)))

(defmethod tree-selected-value ((tree tree-view))
  "Get value of selected node."
  (let ((node (tree-selected-node tree)))
    (when node (node-value node))))

(defmethod tree-handle-key ((tree tree-view) key-event)
  "Handle key event. Returns T if handled."
  (let ((code (key-event-code key-event))
        (char (key-event-char key-event)))
    (cond
      ((or (eql code +key-up+) (eql char #\k))
       (tree-select-prev tree) t)
      ((or (eql code +key-down+) (eql char #\j))
       (tree-select-next tree) t)
      ((or (eql code +key-right+) (eql char #\l))
       (tree-expand tree) t)
      ((or (eql code +key-left+) (eql char #\h))
       (tree-collapse tree) t)
      ((or (eql code +key-enter+) (eql char #\Space))
       (tree-toggle-expand tree) t)
      ((eql char #\*)
       (tree-expand-all tree) t)
      ((eql char #\-)
       (tree-collapse-all tree) t)
      (t nil))))

(defmethod panel-render ((tree tree-view))
  "Render the tree view."
  (when (panel-visible-p tree)
    (let* ((x (panel-x tree))
           (y (panel-y tree))
           (w (panel-width tree))
           (h (panel-height tree))
           (border-p (panel-border-p tree))
           (indent (tree-indent-size tree)))
      ;; Recompute visible nodes
      (tree-compute-visible tree)
      (let ((nodes (tree-visible-nodes tree))
            (selected (tree-selected-node tree))
            (offset (tree-scroll-offset tree))
            (visible (tree-visible-count tree))
            (content-x (if border-p (1+ x) x))
            (content-y (if border-p (1+ y) y))
            (content-w (if border-p (- w 2) w)))
        ;; Draw border
        (when border-p
          (draw-box x y w h))
        ;; Draw nodes
        (loop for i from offset below (min (length nodes) (+ offset visible))
              for entry = (nth i nodes)
              for node = (car entry)
              for depth = (cdr entry)
              for row = (+ content-y (- i offset))
              for is-selected = (eq node selected)
              do (cursor-to row content-x)
                 (when is-selected (reverse-video))
                 ;; Indent
                 (loop repeat (* depth indent) do (write-char #\Space *terminal-io*))
                 ;; Expand indicator
                 (cond
                   ((node-leaf-p node)
                    (write-char #\Space *terminal-io*))
                   ((node-expanded-p node)
                    (write-char #\▼ *terminal-io*))
                   (t
                    (write-char #\▶ *terminal-io*)))
                 (write-char #\Space *terminal-io*)
                 ;; Label (truncate if needed)
                 (let* ((prefix-len (+ (* depth indent) 2))
                        (max-label (- content-w prefix-len))
                        (label (node-label node))
                        (display (if (> (length label) max-label)
                                     (subseq label 0 (max 0 max-label))
                                     label)))
                   (princ display *terminal-io*)
                   ;; Pad to clear line
                   (loop repeat (- content-w prefix-len (length display))
                         do (write-char #\Space *terminal-io*)))
                 (when is-selected (reset)))
        (force-output *terminal-io*)))
    (setf (panel-dirty-p tree) nil)))
