;;; tests.lisp - Test suite for Charmed

(defpackage #:charmed/tests
  (:use #:cl #:charmed #:parachute)
  (:export #:run-tests))

(in-package #:charmed/tests)

(define-test charmed-tests)

;;; ============================================================
;;; Color Tests
;;; ============================================================

(define-test color-tests
  :parent charmed-tests)

(define-test indexed-color-creation
  :parent color-tests
  (let ((color (make-indexed-color 42)))
    (is eq 'indexed-color (type-of color))
    (is = 42 (color-index color))))

(define-test rgb-color-creation
  :parent color-tests
  (let ((color (make-rgb-color 255 128 64)))
    (is eq 'rgb-color (type-of color))
    (is = 255 (color-red color))
    (is = 128 (color-green color))
    (is = 64 (color-blue color))))

(define-test named-color-creation
  :parent color-tests
  (let ((color (make-named-color :test-color 100)))
    (is eq 'named-color (type-of color))
    (is eq :test-color (color-name color))
    (is = 100 (color-index color))))

(define-test color-palette-lookup
  :parent color-tests
  (let ((red (lookup-color :red)))
    (is eq 'named-color (type-of red))
    (is = 1 (color-index red)))
  (let ((idx (lookup-color 42)))
    (is eq 'indexed-color (type-of idx))
    (is = 42 (color-index idx)))
  (is eq nil (lookup-color nil)))

;;; ============================================================
;;; Style Tests
;;; ============================================================

(define-test style-tests
  :parent charmed-tests)

(define-test style-creation
  :parent style-tests
  (let ((style (make-style :fg :red :bg :blue :bold t)))
    (is eq 'text-style (type-of style))
    (true (style-bold-p style))
    (false (style-italic-p style))
    (is = 1 (color-index (style-fg style)))
    (is = 4 (color-index (style-bg style)))))

;;; ============================================================
;;; Key Event Tests
;;; ============================================================

(define-test key-event-tests
  :parent charmed-tests)

(define-test key-event-creation
  :parent key-event-tests
  (let ((key (make-key-event :char #\a)))
    (is eq 'key-event (type-of key))
    (is eql #\a (key-event-char key))
    (is eq nil (key-event-code key))
    (false (key-event-ctrl-p key))
    (false (key-event-alt-p key))))

(define-test key-event-with-modifiers
  :parent key-event-tests
  (let ((key (make-key-event :char #\c :ctrl-p t)))
    (is eql #\c (key-event-char key))
    (true (key-event-ctrl-p key)))
  (let ((key (make-key-event :code +key-up+ :alt-p t)))
    (is eq +key-up+ (key-event-code key))
    (true (key-event-alt-p key))))

(define-test key-event-mouse
  :parent key-event-tests
  (let ((key (make-key-event :code +key-mouse+ :mouse-x 10 :mouse-y 20)))
    (is eq +key-mouse+ (key-event-code key))
    (is = 10 (key-event-mouse-x key))
    (is = 20 (key-event-mouse-y key))))

;;; ============================================================
;;; Widget Tests
;;; ============================================================

(define-test widget-tests
  :parent charmed-tests)

(define-test panel-creation
  :parent widget-tests
  (let ((panel (make-instance 'panel :x 5 :y 10 :width 40 :height 20)))
    (is = 5 (panel-x panel))
    (is = 10 (panel-y panel))
    (is = 40 (panel-width panel))
    (is = 20 (panel-height panel))
    (true (panel-border-p panel))
    (true (panel-visible-p panel))))

(define-test panel-content-dimensions
  :parent widget-tests
  (let ((panel (make-instance 'panel :x 1 :y 1 :width 40 :height 20 :border t)))
    (is = 2 (panel-content-x panel))
    (is = 2 (panel-content-y panel))
    (is = 38 (panel-content-width panel))
    (is = 18 (panel-content-height panel)))
  (let ((panel (make-instance 'panel :x 1 :y 1 :width 40 :height 20 :border nil)))
    (is = 1 (panel-content-x panel))
    (is = 1 (panel-content-y panel))
    (is = 40 (panel-content-width panel))
    (is = 20 (panel-content-height panel))))

(define-test scrollable-list-creation
  :parent widget-tests
  (let ((list (make-instance 'scrollable-list
                             :x 1 :y 1 :width 40 :height 10
                             :items '("a" "b" "c" "d" "e"))))
    (is = 5 (length (list-items list)))
    (is = 0 (list-cursor list))
    (is = 0 (list-scroll-offset list))
    (is string= "a" (list-selected-item list))))

(define-test scrollable-list-cursor-movement
  :parent widget-tests
  (let ((list (make-instance 'scrollable-list
                             :x 1 :y 1 :width 40 :height 5
                             :items '("a" "b" "c" "d" "e"))))
    (list-move-cursor list 1)
    (is = 1 (list-cursor list))
    (is string= "b" (list-selected-item list))
    (list-move-cursor list 1)
    (is = 2 (list-cursor list))
    (list-move-cursor list -1)
    (is = 1 (list-cursor list))
    ;; Can't go below 0
    (list-move-cursor list -10)
    (is = 1 (list-cursor list))))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(define-test utility-tests
  :parent charmed-tests)

(define-test string-truncation
  :parent utility-tests
  (is string= "hello" (truncate-string "hello" 10))
  (is string= "hel…" (truncate-string "hello" 4))
  (is string= "h…" (truncate-string "hello" 2)))

(define-test string-padding
  :parent utility-tests
  (is string= "hello     " (pad-string "hello" 10))
  (is string= "hello" (pad-string "hello" 5))
  (is string= "hell" (pad-string "hello" 4)))

(define-test text-alignment
  :parent utility-tests
  (is string= "hello     " (align-text "hello" 10 :left))
  (is string= "     hello" (align-text "hello" 10 :right))
  (is string= "  hello   " (align-text "hello" 10 :center)))

;;; ============================================================
;;; Text Input Tests
;;; ============================================================

(define-test text-input-tests
  :parent widget-tests)

(define-test text-input-creation
  :parent text-input-tests
  (let ((input (make-instance 'text-input :width 20 :placeholder "Enter text")))
    (is string= "" (input-value input))
    (is = 0 (input-cursor-pos input))
    (is string= "Enter text" (input-placeholder input))))

(define-test text-input-insertion
  :parent text-input-tests
  (let ((input (make-instance 'text-input :width 20)))
    (input-insert-char input #\h)
    (input-insert-char input #\i)
    (is string= "hi" (input-value input))
    (is = 2 (input-cursor-pos input))))

(define-test text-input-deletion
  :parent text-input-tests
  (let ((input (make-instance 'text-input :width 20 :value "hello" :cursor-pos 5)))
    (input-delete-char input)
    (is string= "hell" (input-value input))
    (is = 4 (input-cursor-pos input))
    (input-delete-char input)
    (is string= "hel" (input-value input))))

(define-test text-input-cursor-movement
  :parent text-input-tests
  (let ((input (make-instance 'text-input :width 20 :value "hello" :cursor-pos 2)))
    (input-move-cursor input 1)
    (is = 3 (input-cursor-pos input))
    (input-move-cursor input -1)
    (is = 2 (input-cursor-pos input))
    (input-move-to-start input)
    (is = 0 (input-cursor-pos input))
    (input-move-to-end input)
    (is = 5 (input-cursor-pos input))))

(define-test text-input-max-length
  :parent text-input-tests
  (let ((input (make-instance 'text-input :width 20 :max-length 3)))
    (input-insert-char input #\a)
    (input-insert-char input #\b)
    (input-insert-char input #\c)
    (input-insert-char input #\d)  ; Should be ignored
    (is string= "abc" (input-value input))
    (is = 3 (input-cursor-pos input))))

;;; ============================================================
;;; Progress Bar Tests
;;; ============================================================

(define-test progress-bar-tests
  :parent widget-tests)

(define-test progress-bar-creation
  :parent progress-bar-tests
  (let ((bar (make-instance 'progress-bar :width 20 :progress 0.5)))
    (is = 0.5 (progress-value bar))
    (true (progress-show-percentage-p bar))))

(define-test progress-bar-set
  :parent progress-bar-tests
  (let ((bar (make-instance 'progress-bar :width 20)))
    (progress-set bar 0.75)
    (is = 0.75 (progress-value bar))
    ;; Clamp to range
    (progress-set bar 1.5)
    (is = 1.0 (progress-value bar))
    (progress-set bar -0.5)
    (is = 0.0 (progress-value bar))))

(define-test progress-bar-indeterminate
  :parent progress-bar-tests
  (let ((bar (make-instance 'progress-bar :width 20 :progress nil)))
    (is eq nil (progress-value bar))
    (is = 0 (progress-indeterminate-pos bar))
    (progress-tick bar)
    (is = 1 (progress-indeterminate-pos bar))))

;;; ============================================================
;;; Status Bar Tests
;;; ============================================================

(define-test status-bar-tests
  :parent widget-tests)

(define-test status-bar-creation
  :parent status-bar-tests
  (let ((bar (make-instance 'status-bar 
                            :width 80
                            :sections '(("Left" . 20) ("Center" . :flex) ("Right" . 20)))))
    (is = 3 (length (status-sections bar)))
    (is string= " │ " (status-separator bar))))

(define-test status-bar-set-section
  :parent status-bar-tests
  (let ((bar (make-instance 'status-bar 
                            :width 80
                            :sections '(("A" . 10) ("B" . 10)))))
    (status-set-section bar 0 "New A")
    (is string= "New A" (car (first (status-sections bar))))
    (status-set-section bar 1 "New B")
    (is string= "New B" (car (second (status-sections bar))))))

;;; ============================================================
;;; Layout Tests
;;; ============================================================

(define-test layout-tests
  :parent widget-tests)

(define-test layout-creation
  :parent layout-tests
  (let ((layout (make-instance 'layout :x 1 :y 1 :width 80 :height 24)))
    (is = 1 (layout-x layout))
    (is = 1 (layout-y layout))
    (is = 80 (layout-width layout))
    (is = 24 (layout-height layout))
    (is eq :horizontal (layout-direction layout))))

(define-test layout-horizontal-children
  :parent layout-tests
  (let ((layout (make-instance 'layout :x 1 :y 1 :width 80 :height 24 :direction :horizontal))
        (p1 (make-instance 'panel))
        (p2 (make-instance 'panel)))
    (layout-add-child layout p1 30)
    (layout-add-child layout p2 :flex)
    ;; p1 should be at x=1, width=30
    (is = 1 (panel-x p1))
    (is = 30 (panel-width p1))
    (is = 24 (panel-height p1))
    ;; p2 should be at x=31, width=50 (80-30)
    (is = 31 (panel-x p2))
    (is = 50 (panel-width p2))))

(define-test layout-vertical-children
  :parent layout-tests
  (let ((layout (make-instance 'layout :x 1 :y 1 :width 80 :height 24 :direction :vertical))
        (p1 (make-instance 'panel))
        (p2 (make-instance 'panel)))
    (layout-add-child layout p1 10)
    (layout-add-child layout p2 :flex)
    ;; p1 should be at y=1, height=10
    (is = 1 (panel-y p1))
    (is = 10 (panel-height p1))
    (is = 80 (panel-width p1))
    ;; p2 should be at y=11, height=14 (24-10)
    (is = 11 (panel-y p2))
    (is = 14 (panel-height p2))))

(define-test layout-resize
  :parent layout-tests
  (let ((layout (make-instance 'layout :x 1 :y 1 :width 80 :height 24 :direction :horizontal))
        (p1 (make-instance 'panel))
        (p2 (make-instance 'panel)))
    (layout-add-child layout p1 :flex)
    (layout-add-child layout p2 :flex)
    ;; Initially each gets 40
    (is = 40 (panel-width p1))
    (is = 40 (panel-width p2))
    ;; Resize to 100
    (layout-resize layout 100 24)
    (is = 50 (panel-width p1))
    (is = 50 (panel-width p2))))

;;; ============================================================
;;; Split Pane Tests
;;; ============================================================

(define-test split-pane-tests
  :parent widget-tests)

(define-test split-pane-creation
  :parent split-pane-tests
  (let* ((p1 (make-instance 'panel))
         (p2 (make-instance 'panel))
         (split (make-instance 'split-pane 
                               :x 1 :y 1 :width 81 :height 24
                               :first p1 :second p2 :split-pos 0.5)))
    ;; With divider, 80 available, split at 50%
    (is = 40 (panel-width p1))
    (is = 40 (panel-width p2))
    (is = 1 (panel-x p1))
    (is = 42 (panel-x p2))))  ; 1 + 40 + 1 (divider)

(define-test split-pane-adjust
  :parent split-pane-tests
  (let* ((p1 (make-instance 'panel))
         (p2 (make-instance 'panel))
         (split (make-instance 'split-pane 
                               :x 1 :y 1 :width 81 :height 24
                               :first p1 :second p2 :split-pos 0.5)))
    ;; Adjust by +10
    (split-adjust split 10)
    (is = 50 (panel-width p1))
    (is = 30 (panel-width p2))))

(define-test split-pane-vertical
  :parent split-pane-tests
  (let* ((p1 (make-instance 'panel))
         (p2 (make-instance 'panel))
         (split (make-instance 'split-pane 
                               :x 1 :y 1 :width 80 :height 25
                               :direction :vertical
                               :first p1 :second p2 :split-pos 0.5)))
    ;; With divider, 24 available, split at 50%
    (is = 12 (panel-height p1))
    (is = 12 (panel-height p2))
    (is = 1 (panel-y p1))
    (is = 14 (panel-y p2))))  ; 1 + 12 + 1 (divider)

;;; ============================================================
;;; Resize Handling Tests
;;; ============================================================

(define-test resize-tests
  :parent charmed-tests)

(define-test resize-initial-state
  :parent resize-tests
  ;; Initially no resize pending
  (setf *resize-pending* nil)
  (false *resize-pending*)
  (is eq nil (check-resize)))

(define-test resize-pending-flag
  :parent resize-tests
  ;; Simulate resize by calling on-resize directly
  (setf *resize-pending* nil)
  (charmed::on-resize 120 40)
  (true *resize-pending*)
  (is = 120 *last-width*)
  (is = 40 *last-height*))

(define-test resize-check-clears-flag
  :parent resize-tests
  ;; check-resize should clear the pending flag
  (setf *resize-pending* nil)
  (charmed::on-resize 100 30)
  (true *resize-pending*)
  (multiple-value-bind (w h) (check-resize)
    (is = 100 w)
    (is = 30 h))
  (false *resize-pending*))

(define-test resize-poll-returns-key-event
  :parent resize-tests
  ;; poll-resize should return a key-event with resize code
  (setf *resize-pending* nil)
  (charmed::on-resize 80 24)
  (let ((event (poll-resize)))
    (is eq +key-resize+ (key-event-code event))
    (is = 80 (key-event-mouse-x event))
    (is = 24 (key-event-mouse-y event))))

;;; ============================================================
;;; Screen Buffer Tests
;;; ============================================================

(define-test screen-buffer-tests
  :parent charmed-tests)

(define-test cell-creation
  :parent screen-buffer-tests
  (let ((cell (make-cell #\A)))
    (is eql #\A (cell-char cell))
    (is eq nil (cell-fg cell))
    (is eq nil (cell-bg cell))))

(define-test cell-equality
  :parent screen-buffer-tests
  (let ((c1 (make-cell #\A))
        (c2 (make-cell #\A))
        (c3 (make-cell #\B)))
    (true (cell-equal c1 c2))
    (false (cell-equal c1 c3))))

(define-test buffer-creation
  :parent screen-buffer-tests
  (let ((buf (make-instance 'screen-buffer :width 40 :height 10)))
    (is = 40 (buffer-width buf))
    (is = 10 (buffer-height buf))
    (is eql #\Space (cell-char (buffer-get-cell buf 1 1)))))

(define-test buffer-set-get-cell
  :parent screen-buffer-tests
  (let ((buf (make-instance 'screen-buffer :width 40 :height 10)))
    (buffer-set-cell buf 5 3 #\X)
    (is eql #\X (cell-char (buffer-get-cell buf 5 3)))
    ;; Out of bounds returns nil
    (is eq nil (buffer-get-cell buf 100 100))))

(define-test buffer-write-string-test
  :parent screen-buffer-tests
  (let ((buf (make-instance 'screen-buffer :width 40 :height 10)))
    (buffer-write-string buf 1 1 "Hello")
    (is eql #\H (cell-char (buffer-get-cell buf 1 1)))
    (is eql #\e (cell-char (buffer-get-cell buf 2 1)))
    (is eql #\l (cell-char (buffer-get-cell buf 3 1)))
    (is eql #\l (cell-char (buffer-get-cell buf 4 1)))
    (is eql #\o (cell-char (buffer-get-cell buf 5 1)))))

(define-test buffer-resize-test
  :parent screen-buffer-tests
  (let ((buf (make-instance 'screen-buffer :width 40 :height 10)))
    (buffer-set-cell buf 5 3 #\X)
    (buffer-resize buf 80 24)
    (is = 80 (buffer-width buf))
    (is = 24 (buffer-height buf))
    ;; Old content preserved
    (is eql #\X (cell-char (buffer-get-cell buf 5 3)))))

(define-test screen-creation
  :parent screen-buffer-tests
  (let ((scr (make-instance 'screen :width 80 :height 24)))
    (is = 80 (screen-width scr))
    (is = 24 (screen-height scr))
    (true (screen-front scr))
    (true (screen-back scr))))

(define-test screen-write-operations
  :parent screen-buffer-tests
  (let ((scr (make-instance 'screen :width 80 :height 24)))
    (screen-set-cell scr 10 5 #\Z)
    (is eql #\Z (cell-char (buffer-get-cell (screen-back scr) 10 5)))
    (screen-write-string scr 1 1 "Test")
    (is eql #\T (cell-char (buffer-get-cell (screen-back scr) 1 1)))))

;;; ============================================================
;;; Modal Dialog Tests
;;; ============================================================

(define-test modal-dialog-tests
  :parent charmed-tests)

(define-test dialog-creation
  :parent modal-dialog-tests
  (let ((dialog (make-instance 'modal-dialog
                               :x 10 :y 5 :width 40 :height 10
                               :message "Test message"
                               :buttons '("OK" "Cancel"))))
    (is string= "Test message" (dialog-message dialog))
    (is = 2 (length (dialog-buttons dialog)))
    (is = 0 (dialog-selected-button dialog))
    (false (dialog-closed-p dialog))))

(define-test dialog-button-navigation
  :parent modal-dialog-tests
  (let ((dialog (make-instance 'modal-dialog
                               :x 10 :y 5 :width 40 :height 10
                               :buttons '("A" "B" "C"))))
    (is = 0 (dialog-selected-button dialog))
    (dialog-select-next dialog)
    (is = 1 (dialog-selected-button dialog))
    (dialog-select-next dialog)
    (is = 2 (dialog-selected-button dialog))
    (dialog-select-next dialog)
    (is = 0 (dialog-selected-button dialog))  ; Wraps around
    (dialog-select-prev dialog)
    (is = 2 (dialog-selected-button dialog))))

(define-test dialog-confirm-cancel
  :parent modal-dialog-tests
  (let ((dialog (make-instance 'modal-dialog
                               :x 10 :y 5 :width 40 :height 10
                               :buttons '("Yes" "No"))))
    (dialog-select-next dialog)
    (dialog-confirm dialog)
    (is = 1 (dialog-result dialog))
    (true (dialog-closed-p dialog)))
  (let ((dialog (make-instance 'modal-dialog
                               :x 10 :y 5 :width 40 :height 10
                               :buttons '("Yes" "No"))))
    (dialog-cancel dialog)
    (is eq nil (dialog-result dialog))
    (true (dialog-closed-p dialog))))

(define-test text-wrapping
  :parent modal-dialog-tests
  (let ((lines (wrap-text "Hello world this is a test" 10)))
    (is = 3 (length lines))
    (is string= "Hello" (first lines))))

(define-test string-splitting
  :parent modal-dialog-tests
  (let ((parts (split-string "one two three")))
    (is = 3 (length parts))
    (is string= "one" (first parts))
    (is string= "three" (third parts))))

(define-test input-dialog-creation
  :parent modal-dialog-tests
  (let ((dialog (make-instance 'input-dialog
                               :x 10 :y 5 :width 50 :height 8
                               :prompt "Enter name:")))
    (is string= "Enter name:" (dialog-prompt dialog))
    (true (dialog-input dialog))
    (is = 2 (length (dialog-buttons dialog)))))

;;; ============================================================
;;; Menu Tests
;;; ============================================================

(define-test menu-tests
  :parent charmed-tests)

(define-test menu-item-creation
  :parent menu-tests
  (let ((item (make-menu-item "Open" :shortcut #\o :value :open)))
    (is string= "Open" (menu-item-label item))
    (is eq :open (menu-item-value item))
    (is eql #\o (menu-item-shortcut item))
    (true (menu-item-enabled-p item))
    (false (menu-item-separator-p item))))

(define-test separator-creation
  :parent menu-tests
  (let ((sep (make-separator)))
    (true (menu-item-separator-p sep))))

(define-test menu-creation
  :parent menu-tests
  (let* ((items (list (make-menu-item "New")
                      (make-menu-item "Open")
                      (make-menu-item "Save")))
         (menu (make-instance 'menu :items items :x 1 :y 1 :width 20 :height 5)))
    (is = 3 (length (menu-items menu)))
    (is = 0 (menu-selected-index menu))
    (false (menu-closed-p menu))))

(define-test menu-navigation
  :parent menu-tests
  (let* ((items (list (make-menu-item "A")
                      (make-menu-item "B")
                      (make-menu-item "C")))
         (menu (make-instance 'menu :items items :x 1 :y 1 :width 20 :height 5)))
    (is = 0 (menu-selected-index menu))
    (menu-select-next menu)
    (is = 1 (menu-selected-index menu))
    (menu-select-next menu)
    (is = 2 (menu-selected-index menu))
    (menu-select-next menu)
    (is = 0 (menu-selected-index menu))  ; Wraps
    (menu-select-prev menu)
    (is = 2 (menu-selected-index menu))))

(define-test menu-skips-separators
  :parent menu-tests
  (let* ((items (list (make-menu-item "A")
                      (make-separator)
                      (make-menu-item "B")))
         (menu (make-instance 'menu :items items :x 1 :y 1 :width 20 :height 5)))
    (is = 0 (menu-selected-index menu))
    (menu-select-next menu)
    (is = 2 (menu-selected-index menu))))  ; Skips separator

(define-test menu-skips-disabled
  :parent menu-tests
  (let* ((items (list (make-menu-item "A")
                      (make-menu-item "B" :enabled nil)
                      (make-menu-item "C")))
         (menu (make-instance 'menu :items items :x 1 :y 1 :width 20 :height 5)))
    (menu-select-next menu)
    (is = 2 (menu-selected-index menu))))  ; Skips disabled

(define-test menu-confirm-cancel
  :parent menu-tests
  (let* ((items (list (make-menu-item "A" :value :a)
                      (make-menu-item "B" :value :b)))
         (menu (make-instance 'menu :items items :x 1 :y 1 :width 20 :height 4)))
    (menu-select-next menu)
    (menu-confirm menu)
    (is eq :b (menu-result menu))
    (true (menu-closed-p menu)))
  (let* ((items (list (make-menu-item "A")))
         (menu (make-instance 'menu :items items :x 1 :y 1 :width 20 :height 3)))
    (menu-cancel menu)
    (is eq nil (menu-result menu))
    (true (menu-closed-p menu))))

(define-test menu-bar-creation
  :parent menu-tests
  (let ((bar (make-instance 'menu-bar
                            :menus '(("File" . nil) ("Edit" . nil))
                            :x 1 :y 1 :width 80)))
    (is = 2 (length (menu-bar-menus bar)))
    (is = 0 (menu-bar-selected-index bar))
    (false (menu-bar-active-p bar))))

(define-test menu-bar-navigation
  :parent menu-tests
  (let ((bar (make-instance 'menu-bar
                            :menus '(("File" . nil) ("Edit" . nil) ("View" . nil))
                            :x 1 :y 1 :width 80)))
    (menu-bar-select-next bar)
    (is = 1 (menu-bar-selected-index bar))
    (menu-bar-select-prev bar)
    (is = 0 (menu-bar-selected-index bar))
    (menu-bar-select-prev bar)
    (is = 2 (menu-bar-selected-index bar))))  ; Wraps

;;; ============================================================
;;; Table Tests
;;; ============================================================

(define-test table-tests
  :parent charmed-tests)

(define-test column-creation
  :parent table-tests
  (let ((col (make-column "Name" :name :width 20 :align :left)))
    (is string= "Name" (column-header col))
    (is eq :name (column-key col))
    (is = 20 (column-width col))
    (is eq :left (column-align col))))

(define-test table-creation
  :parent table-tests
  (let* ((cols (list (make-column "Name" :name)
                     (make-column "Age" :age :align :right)))
         (rows (list '(:name "Alice" :age 30)
                     '(:name "Bob" :age 25)))
         (table (make-instance 'table-widget
                               :columns cols :rows rows
                               :x 1 :y 1 :width 40 :height 10)))
    (is = 2 (length (table-columns table)))
    (is = 2 (length (table-rows table)))
    (is = 0 (table-selected-row table))))

(define-test table-navigation
  :parent table-tests
  (let* ((cols (list (make-column "X" :x)))
         (rows (list '(:x 1) '(:x 2) '(:x 3) '(:x 4) '(:x 5)))
         (table (make-instance 'table-widget
                               :columns cols :rows rows
                               :x 1 :y 1 :width 20 :height 10)))
    (is = 0 (table-selected-row table))
    (table-select-next table)
    (is = 1 (table-selected-row table))
    (table-select-last table)
    (is = 4 (table-selected-row table))
    (table-select-first table)
    (is = 0 (table-selected-row table))
    (table-select-prev table)
    (is = 0 (table-selected-row table))))  ; Can't go below 0

(define-test table-selected-data
  :parent table-tests
  (let* ((cols (list (make-column "Name" :name)))
         (rows (list '(:name "Alice") '(:name "Bob")))
         (table (make-instance 'table-widget
                               :columns cols :rows rows
                               :x 1 :y 1 :width 20 :height 10)))
    (is equal '(:name "Alice") (table-selected-data table))
    (table-select-next table)
    (is equal '(:name "Bob") (table-selected-data table))))

(define-test get-cell-value-plist
  :parent table-tests
  (let ((col (make-column "Name" :name))
        (row '(:name "Alice" :age 30)))
    (is string= "Alice" (get-cell-value row col))))

(define-test get-cell-value-alist
  :parent table-tests
  (let ((col (make-column "Name" :name))
        (row '((:name . "Bob") (:age . 25))))
    (is string= "Bob" (get-cell-value row col))))

(define-test align-string-test
  :parent table-tests
  (is string= "foo  " (align-string "foo" 5 :left))
  (is string= "  foo" (align-string "foo" 5 :right))
  (is string= " foo " (align-string "foo" 5 :center))
  (is string= "foo" (align-string "foobar" 3 :left)))  ; Truncates

(define-test table-sorting
  :parent table-tests
  (let* ((cols (list (make-column "Name" :name)))
         (rows (list '(:name "Charlie") '(:name "Alice") '(:name "Bob")))
         (table (make-instance 'table-widget
                               :columns cols :rows rows
                               :sortable t
                               :x 1 :y 1 :width 20 :height 10)))
    (table-sort-by table 0)
    (is string= "Alice" (getf (first (table-rows table)) :name))
    (true (table-sort-ascending-p table))
    (table-sort-by table 0)  ; Toggle to descending
    (is string= "Charlie" (getf (first (table-rows table)) :name))
    (false (table-sort-ascending-p table))))

;;; ============================================================
;;; Form Tests
;;; ============================================================

(define-test form-tests
  :parent charmed-tests)

(define-test field-creation
  :parent form-tests
  (let ((field (make-field "Username" :username :required t :width 25)))
    (is string= "Username" (field-label field))
    (is eq :username (field-key field))
    (true (field-required-p field))
    (true (field-widget field))))

(define-test field-value-access
  :parent form-tests
  (let ((field (make-field "Name" :name :value "Alice")))
    (is string= "Alice" (field-value field))
    (setf (field-value field) "Bob")
    (is string= "Bob" (field-value field))))

(define-test field-validation-required
  :parent form-tests
  (let ((field (make-field "Name" :name :required t)))
    (false (field-validate field))
    (is string= "Required" (field-error-message field))
    (setf (field-value field) "Alice")
    (true (field-validate field))
    (is eq nil (field-error-message field))))

(define-test field-validation-custom
  :parent form-tests
  (let ((field (make-field "Age" :age 
                           :validator (lambda (v) 
                                        (and v (> (length v) 0)
                                             (every #'digit-char-p v))))))
    (setf (field-value field) "abc")
    (false (field-validate field))
    (is string= "Invalid" (field-error-message field))
    (setf (field-value field) "25")
    (true (field-validate field))))

(define-test form-creation
  :parent form-tests
  (let* ((fields (list (make-field "Name" :name)
                       (make-field "Email" :email)))
         (form (make-instance 'form-widget
                              :fields fields
                              :x 1 :y 1 :width 60 :height 10)))
    (is = 2 (length (form-fields form)))
    (is = 0 (form-focused-index form))
    (false (form-submitted-p form))
    (false (form-cancelled-p form))))

(define-test form-navigation
  :parent form-tests
  (let* ((fields (list (make-field "A" :a)
                       (make-field "B" :b)
                       (make-field "C" :c)))
         (form (make-instance 'form-widget
                              :fields fields
                              :x 1 :y 1 :width 60 :height 10)))
    (is = 0 (form-focused-index form))
    (form-focus-next form)
    (is = 1 (form-focused-index form))
    (form-focus-next form)
    (is = 2 (form-focused-index form))
    (form-focus-next form)
    (is = 0 (form-focused-index form))  ; Wraps
    (form-focus-prev form)
    (is = 2 (form-focused-index form))))

(define-test form-values-retrieval
  :parent form-tests
  (let* ((fields (list (make-field "Name" :name :value "Alice")
                       (make-field "City" :city :value "Boston")))
         (form (make-instance 'form-widget
                              :fields fields
                              :x 1 :y 1 :width 60 :height 10)))
    (let ((values (form-values form)))
      (is string= "Alice" (getf values :name))
      (is string= "Boston" (getf values :city)))))

(define-test form-validation
  :parent form-tests
  (let* ((fields (list (make-field "Name" :name :required t)
                       (make-field "Email" :email)))
         (form (make-instance 'form-widget
                              :fields fields
                              :x 1 :y 1 :width 60 :height 10)))
    (false (form-validate form))  ; Name is required but empty
    (setf (field-value (first (form-fields form))) "Alice")
    (true (form-validate form))))

(define-test form-submit-cancel
  :parent form-tests
  (let* ((fields (list (make-field "Name" :name :value "Test")))
         (form (make-instance 'form-widget
                              :fields fields
                              :x 1 :y 1 :width 60 :height 10)))
    (form-submit form)
    (true (form-submitted-p form)))
  (let* ((fields (list (make-field "Name" :name)))
         (form (make-instance 'form-widget
                              :fields fields
                              :x 1 :y 1 :width 60 :height 10)))
    (form-cancel form)
    (true (form-cancelled-p form))))

;;; ============================================================
;;; Tree View Tests
;;; ============================================================

(define-test tree-tests
  :parent charmed-tests)

(define-test node-creation
  :parent tree-tests
  (let ((node (make-node "Root" :value :root)))
    (is string= "Root" (node-label node))
    (is eq :root (node-value node))
    (true (node-leaf-p node))
    (false (node-expanded-p node))))

(define-test node-with-children
  :parent tree-tests
  (let* ((child1 (make-node "Child 1"))
         (child2 (make-node "Child 2"))
         (parent (make-node "Parent" :children (list child1 child2))))
    (false (node-leaf-p parent))
    (is = 2 (length (node-children parent)))
    (is eq parent (node-parent child1))
    (is eq parent (node-parent child2))))

(define-test node-depth
  :parent tree-tests
  (let* ((grandchild (make-node "Grandchild"))
         (child (make-node "Child" :children (list grandchild)))
         (root (make-node "Root" :children (list child))))
    (is = 0 (node-depth root))
    (is = 1 (node-depth child))
    (is = 2 (node-depth grandchild))))

(define-test tree-creation
  :parent tree-tests
  (let* ((root (make-node "Root" 
                          :children (list (make-node "A") (make-node "B"))))
         (tree (make-instance 'tree-view
                              :root root
                              :x 1 :y 1 :width 40 :height 20)))
    (is eq root (tree-root tree))
    (true (tree-show-root-p tree))
    (is eq root (tree-selected-node tree))))

(define-test tree-navigation
  :parent tree-tests
  (let* ((a (make-node "A"))
         (b (make-node "B"))
         (root (make-node "Root" :children (list a b) :expanded t))
         (tree (make-instance 'tree-view
                              :root root
                              :x 1 :y 1 :width 40 :height 20)))
    (is eq root (tree-selected-node tree))
    (tree-select-next tree)
    (is eq a (tree-selected-node tree))
    (tree-select-next tree)
    (is eq b (tree-selected-node tree))
    (tree-select-prev tree)
    (is eq a (tree-selected-node tree))))

(define-test tree-expand-collapse
  :parent tree-tests
  (let* ((child (make-node "Child"))
         (root (make-node "Root" :children (list child)))
         (tree (make-instance 'tree-view
                              :root root
                              :x 1 :y 1 :width 40 :height 20)))
    (false (node-expanded-p root))
    (tree-expand tree)
    (true (node-expanded-p root))
    (tree-collapse tree)
    (false (node-expanded-p root))
    (tree-toggle-expand tree)
    (true (node-expanded-p root))))

(define-test tree-selected-value
  :parent tree-tests
  (let* ((root (make-node "Root" :value :root-value))
         (tree (make-instance 'tree-view
                              :root root
                              :x 1 :y 1 :width 40 :height 20)))
    (is eq :root-value (tree-selected-value tree))))

(define-test tree-visible-nodes
  :parent tree-tests
  (let* ((a (make-node "A"))
         (b (make-node "B"))
         (root (make-node "Root" :children (list a b)))
         (tree (make-instance 'tree-view
                              :root root
                              :x 1 :y 1 :width 40 :height 20)))
    ;; Initially only root visible (not expanded)
    (tree-compute-visible tree)
    (is = 1 (length (tree-visible-nodes tree)))
    ;; Expand root
    (setf (node-expanded-p root) t)
    (tree-compute-visible tree)
    (is = 3 (length (tree-visible-nodes tree)))))

(define-test tree-expand-collapse-all
  :parent tree-tests
  (let* ((grandchild (make-node "GC"))
         (child (make-node "Child" :children (list grandchild)))
         (root (make-node "Root" :children (list child)))
         (tree (make-instance 'tree-view
                              :root root
                              :x 1 :y 1 :width 40 :height 20)))
    (tree-expand-all tree)
    (true (node-expanded-p root))
    (true (node-expanded-p child))
    (tree-collapse-all tree)
    (false (node-expanded-p root))
    (false (node-expanded-p child))))

;;; ============================================================
;;; Output Buffering Tests
;;; ============================================================

(define-test buffering-tests
  :parent charmed-tests)

(define-test output-buffer-init
  :parent buffering-tests
  (init-output-buffer)
  (true *output-buffer*)
  (setf *output-buffer* nil))

(define-test output-buffer-flush
  :parent buffering-tests
  (let ((test-stream (make-string-output-stream)))
    (init-output-buffer)
    (format *output-buffer* "test output")
    (flush-output-buffer test-stream)
    (is string= "test output" (get-output-stream-string test-stream))
    (setf *output-buffer* nil)))

(define-test output-buffer-clear
  :parent buffering-tests
  (init-output-buffer)
  (format *output-buffer* "test")
  (clear-output-buffer)
  ;; After clear, buffer should be empty
  (is string= "" (get-output-stream-string *output-buffer*))
  (setf *output-buffer* nil))

;;; ============================================================
;;; Accessibility Tests
;;; ============================================================

(define-test accessibility-tests
  :parent charmed-tests)

(define-test accessibility-toggle
  :parent accessibility-tests
  (false *accessibility-enabled*)
  (enable-accessibility)
  (true *accessibility-enabled*)
  (disable-accessibility)
  (false *accessibility-enabled*))

(define-test set-title-test
  :parent accessibility-tests
  (let ((output (make-string-output-stream)))
    (set-title "Test Title" output)
    (let ((result (get-output-stream-string output)))
      ;; Should contain OSC 0 sequence
      (true (search "]0;" result)))))

(define-test announce-when-disabled
  :parent accessibility-tests
  (let ((output (make-string-output-stream)))
    (setf *accessibility-enabled* nil)
    (announce "test" output)
    ;; Should produce no output when disabled
    (is string= "" (get-output-stream-string output))))

(define-test announce-when-enabled
  :parent accessibility-tests
  (let ((output (make-string-output-stream)))
    (setf *accessibility-enabled* t)
    (announce "test message" output)
    (let ((result (get-output-stream-string output)))
      ;; Should contain OSC 777 sequence
      (true (search "]777;" result)))
    (setf *accessibility-enabled* nil)))

;;; ============================================================
;;; Declarative UI DSL Tests
;;; ============================================================

(define-test dsl-tests
  :parent charmed-tests)

;; Reactive State Tests
(define-test state-creation
  :parent dsl-tests
  (let ((s (make-state 42 'test-state)))
    (is = 42 (state-get s))
    (is eq 'test-state (state-name s))))

(define-test state-set-get
  :parent dsl-tests
  (let ((s (make-state 0)))
    (state-set s 10)
    (is = 10 (state-get s))
    (state-set s 20)
    (is = 20 (state-get s))))

(define-test state-update
  :parent dsl-tests
  (let ((s (make-state 5)))
    (state-update s #'1+)
    (is = 6 (state-get s))
    (state-update s (lambda (x) (* x 2)))
    (is = 12 (state-get s))))

(define-test state-subscribe
  :parent dsl-tests
  (let ((s (make-state 0))
        (called nil)
        (received-value nil))
    (state-subscribe s (lambda (v) 
                         (setf called t)
                         (setf received-value v)))
    (state-set s 42)
    (true called)
    (is = 42 received-value)))

(define-test state-unsubscribe
  :parent dsl-tests
  (let ((s (make-state 0))
        (call-count 0))
    (let ((handler (lambda (v) (declare (ignore v)) (incf call-count))))
      (state-subscribe s handler)
      (state-set s 1)
      (is = 1 call-count)
      (state-unsubscribe s handler)
      (state-set s 2)
      (is = 1 call-count))))  ; Should not have been called again

(define-test with-state-macro
  :parent dsl-tests
  (with-state ((count 0) (name "test"))
    (true (typep count 'reactive-state))
    (true (typep name 'reactive-state))
    (is = 0 (state-get count))
    (is string= "test" (state-get name))))

;; VNode Tests
(define-test vnode-creation
  :parent dsl-tests
  (let ((node (make-vnode :type 'panel :props '(:width 10) :children nil)))
    (is eq 'panel (vnode-type node))
    (is equal '(:width 10) (vnode-props node))
    (true (null (vnode-children node)))))

(define-test create-element-test
  :parent dsl-tests
  (let ((el (create-element 'panel '(:width 20 :height 10))))
    (is eq 'panel (vnode-type el))
    (is equal '(:width 20 :height 10) (vnode-props el))))

(define-test create-element-with-children
  :parent dsl-tests
  (let ((el (create-element 'panel '(:width 20)
              (create-element 'text-node '(:value "Hello"))
              (create-element 'text-node '(:value "World")))))
    (is = 2 (length (vnode-children el)))
    (is eq 'text-node (vnode-type (first (vnode-children el))))))

;; Diffing Tests
(define-test diff-identical-vnodes
  :parent dsl-tests
  (let ((a (make-vnode :type 'panel :props '(:width 10)))
        (b (make-vnode :type 'panel :props '(:width 10))))
    (true (null (diff-vnodes a b)))))

(define-test diff-different-props
  :parent dsl-tests
  (let ((a (make-vnode :type 'panel :props '(:width 10)))
        (b (make-vnode :type 'panel :props '(:width 20))))
    (let ((patches (diff-vnodes a b)))
      (is = 1 (length patches))
      (is eq :update-props (first (first patches))))))

(define-test diff-different-types
  :parent dsl-tests
  (let ((a (make-vnode :type 'panel :props nil))
        (b (make-vnode :type 'text-node :props nil)))
    (let ((patches (diff-vnodes a b)))
      (is = 1 (length patches))
      (is eq :replace (first (first patches))))))

(define-test diff-create-node
  :parent dsl-tests
  (let ((patches (diff-vnodes nil (make-vnode :type 'panel))))
    (is = 1 (length patches))
    (is eq :create (first (first patches)))))

(define-test diff-remove-node
  :parent dsl-tests
  (let ((patches (diff-vnodes (make-vnode :type 'panel) nil)))
    (is = 1 (length patches))
    (is eq :remove (first (first patches)))))

;; Event System Tests
(define-test event-register-dispatch
  :parent dsl-tests
  (let ((received nil))
    (let ((id (register-handler :test-event 
                                (lambda (data) (setf received data)))))
      (dispatch-event :test-event "hello")
      (is string= "hello" received)
      (unregister-handler :test-event id))))

;; App Tests
(define-test app-creation
  :parent dsl-tests
  (let ((app (make-app :render (lambda (state) 
                                 (declare (ignore state))
                                 (make-vnode :type 'panel)))))
    (true (typep app 'app))
    (true (functionp (app-render-fn app)))))

;; Button Widget Tests
(define-test button-widget-creation
  :parent dsl-tests
  (let ((btn (make-instance 'button-widget :label "Click Me")))
    (is string= "Click Me" (button-label btn))))

;;; ============================================================
;;; Input Parser Tests
;;; ============================================================

(define-test input-parser-tests
  :parent charmed-tests)

(defun parse-bytes (&rest bytes)
  "Run the key-event parser over a synthetic byte sequence and return
   the resulting KEY-EVENT.  Drives the same code path as a real read by
   binding *READ-BYTE-FN* to a closure that pops from a shared list."
  (let ((queue (copy-list bytes)))
    (let ((charmed:*read-byte-fn* (lambda (fd)
                                    (declare (ignore fd))
                                    (pop queue))))
      ;; FD value is irrelevant - the byte source is the queue.
      (charmed:read-key-event-from-fd 0))))

;;; CSI tilde-terminated function keys (xterm/VT220 PC-style)

(define-test csi-tilde-f1
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 49 49 126))) ; ESC [ 1 1 ~
    (is eq +key-f1+ (key-event-code event))))

(define-test csi-tilde-f2
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 49 50 126))) ; ESC [ 1 2 ~
    (is eq +key-f2+ (key-event-code event))))

(define-test csi-tilde-f3
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 49 51 126))) ; ESC [ 1 3 ~
    (is eq +key-f3+ (key-event-code event))))

(define-test csi-tilde-f4
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 49 52 126))) ; ESC [ 1 4 ~
    (is eq +key-f4+ (key-event-code event))))

(define-test csi-tilde-f5
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 49 53 126))) ; ESC [ 1 5 ~
    (is eq +key-f5+ (key-event-code event))))

(define-test csi-tilde-f6
  :parent input-parser-tests
  ;; The original report: kitty sends ESC [ 1 7 ~ for F6 and the parser
  ;; previously dropped it on the floor as :unknown.
  (let ((event (parse-bytes 27 91 49 55 126)))
    (is eq +key-f6+ (key-event-code event))))

(define-test csi-tilde-f7
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 49 56 126)))
    (is eq +key-f7+ (key-event-code event))))

(define-test csi-tilde-f8
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 49 57 126)))
    (is eq +key-f8+ (key-event-code event))))

(define-test csi-tilde-f9
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 50 48 126)))
    (is eq +key-f9+ (key-event-code event))))

(define-test csi-tilde-f10
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 50 49 126)))
    (is eq +key-f10+ (key-event-code event))))

(define-test csi-tilde-f11
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 50 51 126)))
    (is eq +key-f11+ (key-event-code event))))

(define-test csi-tilde-f12
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 50 52 126)))
    (is eq +key-f12+ (key-event-code event))))

;;; SS3 application-mode F1-F4 (ESC O P/Q/R/S)

(define-test ss3-f1
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 80))) ; ESC O P
    (is eq +key-f1+ (key-event-code event))))

(define-test ss3-f2
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 81))) ; ESC O Q
    (is eq +key-f2+ (key-event-code event))))

(define-test ss3-f3
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 82))) ; ESC O R
    (is eq +key-f3+ (key-event-code event))))

(define-test ss3-f4
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 83))) ; ESC O S
    (is eq +key-f4+ (key-event-code event))))

;;; SS3 cursor cluster - must agree with the existing CSI-form constants

(define-test ss3-cursor-up
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 65))) ; ESC O A
    (is eq +key-up+ (key-event-code event))))

(define-test ss3-cursor-down
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 66))) ; ESC O B
    (is eq +key-down+ (key-event-code event))))

(define-test ss3-cursor-right
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 67))) ; ESC O C
    (is eq +key-right+ (key-event-code event))))

(define-test ss3-cursor-left
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 68))) ; ESC O D
    (is eq +key-left+ (key-event-code event))))

(define-test ss3-cursor-home
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 72))) ; ESC O H
    (is eq +key-home+ (key-event-code event))))

(define-test ss3-cursor-end
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 70))) ; ESC O F
    (is eq +key-end+ (key-event-code event))))

;;; SS3 error paths - timeout (no second byte) and unrecognised final byte

(define-test ss3-timeout-yields-unknown
  :parent input-parser-tests
  ;; *escape-timeout* applies during the read of the second byte. With
  ;; no further bytes queued the wait drops out and returns :unknown.
  (let ((charmed:*escape-timeout* 0.001))
    (let ((event (parse-bytes 27 79))) ; ESC O <nothing>
      (is eq :unknown (key-event-code event)))))

(define-test ss3-unknown-final-byte
  :parent input-parser-tests
  (let ((event (parse-bytes 27 79 90))) ; ESC O Z - unmapped
    (is eq :unknown (key-event-code event))))

;;; Regression guard - existing CSI '~' editing keys still parse correctly

(define-test csi-tilde-delete-still-works
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 51 126))) ; ESC [ 3 ~
    (is eq +key-delete+ (key-event-code event))))

(define-test csi-tilde-page-up-still-works
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 53 126))) ; ESC [ 5 ~
    (is eq +key-page-up+ (key-event-code event))))

(define-test csi-tilde-page-down-still-works
  :parent input-parser-tests
  (let ((event (parse-bytes 27 91 54 126))) ; ESC [ 6 ~
    (is eq +key-page-down+ (key-event-code event))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(defun run-tests ()
  "Run all Charmed tests."
  (test 'charmed-tests))
