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
