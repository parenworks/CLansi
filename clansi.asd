;;; clansi.asd - ASDF system definition for CLansi
;;; A lightweight, pure-Lisp terminal UI library

(asdf:defsystem #:clansi
  :description "Pure-Lisp ANSI terminal control library - no ncurses dependency"
  :author "parenworks"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/parenworks/CLansi"
  :depends-on (#:alexandria
               #:trivial-features)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "compat")
                             (:file "ansi")
                             (:file "terminal")
                             (:file "widgets")
                             (:file "screen")))))

(asdf:defsystem #:clansi/tests
  :description "Test suite for CLansi"
  :author "parenworks"
  :license "MIT"
  :depends-on (#:clansi #:parachute)
  :components ((:file "tests")))
