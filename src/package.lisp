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
   
   ;; Resize handling
   #:*resize-hook*
   #:*resize-pending*
   #:*last-width*
   #:*last-height*
   #:enable-resize-handling
   #:disable-resize-handling
   #:check-resize
   #:poll-resize
   
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
   #:pad-string
   #:align-text
   
   ;; Text input widget
   #:text-input
   #:input-value
   #:input-cursor-pos
   #:input-scroll-offset
   #:input-placeholder
   #:input-max-length
   #:input-password-p
   #:input-insert-char
   #:input-delete-char
   #:input-delete-forward
   #:input-move-cursor
   #:input-move-to-start
   #:input-move-to-end
   #:input-clear
   #:input-handle-key
   
   ;; Progress bar widget
   #:progress-bar
   #:progress-value
   #:progress-style
   #:progress-fill-char
   #:progress-empty-char
   #:progress-show-percentage-p
   #:progress-indeterminate-pos
   #:progress-set
   #:progress-tick
   
   ;; Status bar widget
   #:status-bar
   #:status-sections
   #:status-separator
   #:status-align
   #:status-style
   #:status-set-section
   #:status-set-sections
   
   ;; Layout system
   #:layout
   #:layout-x
   #:layout-y
   #:layout-width
   #:layout-height
   #:layout-children
   #:layout-direction
   #:layout-sizes
   #:layout-gap
   #:layout-visible-p
   #:layout-add-child
   #:layout-remove-child
   #:layout-resize
   #:layout-recalculate
   #:layout-render
   
   ;; Split pane
   #:split-pane
   #:split-first
   #:split-second
   #:split-pos
   #:split-min-size
   #:split-resizable-p
   #:split-show-divider-p
   #:split-divider-char
   #:split-set-children
   #:split-adjust
   
   ;; Screen buffer (double-buffering)
   #:cell
   #:make-cell
   #:cell-char
   #:cell-fg
   #:cell-bg
   #:cell-style
   #:cell-equal
   
   #:screen-buffer
   #:buffer-width
   #:buffer-height
   #:buffer-cells
   #:buffer-cursor-x
   #:buffer-cursor-y
   #:buffer-cursor-visible
   #:buffer-clear
   #:buffer-resize
   #:buffer-set-cell
   #:buffer-get-cell
   #:buffer-write-string
   #:buffer-fill-rect
   
   #:screen
   #:screen-front
   #:screen-back
   #:screen-width
   #:screen-height
   #:screen-stream
   #:screen-resize
   #:screen-clear
   #:screen-set-cell
   #:screen-write-string
   #:screen-fill-rect
   #:screen-set-cursor
   #:screen-show-cursor
   #:screen-present
   #:screen-force-redraw
   
   #:*screen*
   #:init-screen
   #:term-screen
   #:with-screen
   
   ;; Modal dialogs
   #:modal-dialog
   #:dialog-message
   #:dialog-buttons
   #:dialog-selected-button
   #:dialog-result
   #:dialog-closed-p
   #:dialog-select-next
   #:dialog-select-prev
   #:dialog-confirm
   #:dialog-cancel
   #:dialog-handle-key
   
   #:input-dialog
   #:dialog-input
   #:dialog-prompt
   
   ;; Dialog utilities
   #:draw-box
   #:wrap-text
   #:split-string
   #:make-alert
   #:make-confirm
   #:make-input-prompt
   
   ;; Menu system
   #:menu-item
   #:menu-item-label
   #:menu-item-value
   #:menu-item-shortcut
   #:menu-item-enabled-p
   #:menu-item-separator-p
   #:make-menu-item
   #:make-separator
   
   #:menu
   #:menu-items
   #:menu-selected-index
   #:menu-result
   #:menu-closed-p
   #:menu-select-next
   #:menu-select-prev
   #:menu-confirm
   #:menu-cancel
   #:menu-handle-key
   #:menu-selectable-p
   #:compute-menu-dimensions
   #:make-menu
   
   #:menu-bar
   #:menu-bar-menus
   #:menu-bar-selected-index
   #:menu-bar-open-menu
   #:menu-bar-active-p
   #:menu-bar-select-next
   #:menu-bar-select-prev
   #:menu-bar-open
   #:menu-bar-close
   #:menu-bar-handle-key))
