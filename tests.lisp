;;; tests.lisp - Test suite for CLansi

(defpackage #:clansi/tests
  (:use #:cl #:clansi #:parachute)
  (:export #:run-tests))

(in-package #:clansi/tests)

(define-test clansi-tests)

;;; ============================================================
;;; Color Tests
;;; ============================================================

(define-test color-tests
  :parent clansi-tests)

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
  :parent clansi-tests)

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
  :parent clansi-tests)

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
  :parent clansi-tests)

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
  :parent clansi-tests)

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
  :parent clansi-tests)

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
  (clansi::on-resize 120 40)
  (true *resize-pending*)
  (is = 120 *last-width*)
  (is = 40 *last-height*))

(define-test resize-check-clears-flag
  :parent resize-tests
  ;; check-resize should clear the pending flag
  (setf *resize-pending* nil)
  (clansi::on-resize 100 30)
  (true *resize-pending*)
  (multiple-value-bind (w h) (check-resize)
    (is = 100 w)
    (is = 30 h))
  (false *resize-pending*))

(define-test resize-poll-returns-key-event
  :parent resize-tests
  ;; poll-resize should return a key-event with resize code
  (setf *resize-pending* nil)
  (clansi::on-resize 80 24)
  (let ((event (poll-resize)))
    (is eq +key-resize+ (key-event-code event))
    (is = 80 (key-event-mouse-x event))
    (is = 24 (key-event-mouse-y event))))

;;; ============================================================
;;; Screen Buffer Tests
;;; ============================================================

(define-test screen-buffer-tests
  :parent clansi-tests)

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
  :parent clansi-tests)

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
  :parent clansi-tests)

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
;;; Run Tests
;;; ============================================================

(defun run-tests ()
  "Run all CLansi tests."
  (test 'clansi-tests))
