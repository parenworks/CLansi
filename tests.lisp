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

;;; ============================================================
;;; Run Tests
;;; ============================================================

(defun run-tests ()
  "Run all CLansi tests."
  (test 'clansi-tests))
