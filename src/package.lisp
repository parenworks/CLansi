;;; package.lisp - Package definitions for CLansi
;;; A lightweight, pure-Lisp terminal UI library

(defpackage #:clansi
  (:use #:cl)
  (:export
   ;; ANSI escape sequences
   #:*escape*
   
   ;; Color classes
   #:color
   #:indexed-color
   #:rgb-color
   #:named-color
   #:color-index
   #:color-red
   #:color-green
   #:color-blue
   #:color-name
   
   ;; Color constructors
   #:make-indexed-color
   #:make-rgb-color
   #:make-named-color
   #:register-color
   #:lookup-color
   #:*color-palette*
   
   ;; Color output
   #:emit-fg
   #:emit-bg
   #:fg
   #:bg
   #:fg-rgb
   #:bg-rgb
   
   ;; Text styles
   #:text-style
   #:make-style
   #:emit-style
   #:style-fg
   #:style-bg
   #:style-bold-p
   #:style-dim-p
   #:style-italic-p
   #:style-underline-p
   #:style-inverse-p
   
   ;; Style functions
   #:reset
   #:bold
   #:dim
   #:italic
   #:underline
   #:inverse
   #:with-style
   
   ;; Cursor control
   #:cursor-to
   #:cursor-home
   #:cursor-hide
   #:cursor-show
   #:cursor-up
   #:cursor-down
   #:cursor-forward
   #:cursor-back
   
   ;; Screen control
   #:clear-screen
   #:clear-line
   #:clear-to-eol
   #:enter-alternate-screen
   #:leave-alternate-screen
   #:begin-sync-update
   #:end-sync-update
   
   ;; Hyperlinks (OSC 8)
   #:begin-hyperlink
   #:end-hyperlink
   #:hyperlink
   
   ;; Drawing
   #:draw-box
   #:fill-rect
   #:write-at
   #:write-styled
   
   ;; Key events
   #:key-event
   #:make-key-event
   #:key-event-char
   #:key-event-code
   #:key-event-ctrl-p
   #:key-event-alt-p
   #:key-event-mouse-x
   #:key-event-mouse-y
   
   ;; Key codes
   #:+key-up+
   #:+key-down+
   #:+key-left+
   #:+key-right+
   #:+key-enter+
   #:+key-escape+
   #:+key-tab+
   #:+key-backspace+
   #:+key-delete+
   #:+key-home+
   #:+key-end+
   #:+key-page-up+
   #:+key-page-down+
   #:+key-mouse+
   #:+key-resize+
   
   ;; Terminal mode
   #:terminal-mode
   #:*terminal-mode*
   #:terminal-raw-p
   #:terminal-width
   #:terminal-height
   #:enable-raw-mode
   #:disable-raw-mode
   #:query-size
   #:terminal-size
   #:with-raw-terminal
   
   ;; Mouse
   #:enable-mouse-tracking
   #:disable-mouse-tracking
   
   ;; Input
   #:input-reader
   #:*input-reader*
   #:reader-open
   #:reader-close
   #:read-key
   #:read-key-with-timeout
   #:read-key-event
   
   ;; Configuration
   #:*tty-path*
   #:*escape-timeout*
   
   ;; Widgets - Base panel
   #:panel
   #:panel-x
   #:panel-y
   #:panel-width
   #:panel-height
   #:panel-title
   #:panel-active-p
   #:panel-visible-p
   #:panel-border-p
   #:panel-dirty-p
   #:panel-render
   #:panel-clear
   #:panel-content-x
   #:panel-content-y
   #:panel-content-width
   #:panel-content-height
   
   ;; Scrollable list widget
   #:scrollable-list
   #:list-items
   #:list-cursor
   #:list-scroll-offset
   #:list-visible-height
   #:list-move-cursor
   #:list-selected-item
   
   ;; Utilities
   #:truncate-string
   #:pad-string))
