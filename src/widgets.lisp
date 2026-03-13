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
