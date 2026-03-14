;;; charmed.asd - ASDF system definition for Charmed
;;; A lightweight, pure-Lisp terminal UI library

(asdf:defsystem #:charmed
  :description "Charmed - Pure-Lisp ANSI terminal control library"
  :author "parenworks"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/parenworks/charmed"
  :depends-on (#:alexandria
               #:trivial-features)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "compat")
                             (:file "ansi")
                             (:file "terminal")
                             (:file "utils")
                             (:file "widgets")
                             (:file "screen")
                             (:file "dsl")))))

(asdf:defsystem #:charmed/tests
  :description "Test suite for Charmed"
  :author "parenworks"
  :license "MIT"
  :depends-on (#:charmed #:parachute)
  :components ((:file "tests")))
