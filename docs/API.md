# CLansi API Reference

## Overview

CLansi is a terminal UI library for Common Lisp providing ANSI escape sequence handling, keyboard input, and a widget system for building interactive terminal applications.

**Supported Implementations:** SBCL, CCL (ECL has ASDF compatibility issues)

---

## Terminal Control

### Terminal Mode

```lisp
(with-raw-mode &body body)
```

Execute body with terminal in raw mode (no echo, no line buffering). Automatically restores terminal state on exit.

```lisp
(with-alternate-screen &body body)
```

Execute body using the alternate screen buffer. Restores main screen on exit.

### Cursor Control

```lisp
(cursor-to row col)        ; Move cursor to row, col (1-indexed)
(cursor-up &optional n)    ; Move cursor up n rows
(cursor-down &optional n)  ; Move cursor down n rows
(cursor-forward &optional n)   ; Move cursor right n columns
(cursor-backward &optional n)  ; Move cursor left n columns
(cursor-save)              ; Save cursor position
(cursor-restore)           ; Restore saved cursor position
(cursor-hide)              ; Hide cursor
(cursor-show)              ; Show cursor
```

### Screen Control

```lisp
(clear-screen)             ; Clear entire screen
(clear-to-end-of-line)     ; Clear from cursor to end of line
(clear-to-end-of-screen)   ; Clear from cursor to end of screen
(terminal-size)            ; Returns (width height) of terminal
```

---

## Colors and Styles

### Named Colors

```lisp
(make-named-color name)
```

Create a color from name. Supported names:
`:black`, `:red`, `:green`, `:yellow`, `:blue`, `:magenta`, `:cyan`, `:white`,
`:bright-black`, `:bright-red`, `:bright-green`, `:bright-yellow`,
`:bright-blue`, `:bright-magenta`, `:bright-cyan`, `:bright-white`

### RGB Colors

```lisp
(make-rgb-color r g b)     ; r, g, b are 0-255
```

### Applying Colors

```lisp
(fg-color color)           ; Set foreground color
(bg-color color)           ; Set background color
(reset)                    ; Reset all attributes
```

### Text Styles

```lisp
(bold)                     ; Enable bold
(dim)                      ; Enable dim
(italic)                   ; Enable italic
(underline)                ; Enable underline
(blink)                    ; Enable blink
(reverse-video)            ; Enable reverse video
(strikethrough)            ; Enable strikethrough
```

---

## Keyboard Input

### Key Event Structure

```lisp
(key-event-code event)     ; Key code (for special keys)
(key-event-char event)     ; Character (for printable keys)
(key-event-ctrl-p event)   ; T if Ctrl was pressed
(key-event-alt-p event)    ; T if Alt was pressed
(key-event-shift-p event)  ; T if Shift was pressed
```

### Reading Keys

```lisp
(read-key)                 ; Read a key event (blocking)
(read-key-with-timeout ms) ; Read with timeout in milliseconds
                           ; Returns nil on timeout
```

### Key Constants

```lisp
+key-up+ +key-down+ +key-left+ +key-right+
+key-home+ +key-end+ +key-page-up+ +key-page-down+
+key-insert+ +key-delete+ +key-backspace+
+key-enter+ +key-tab+ +key-escape+
+key-f1+ ... +key-f12+
```

---

## Panel (Base Widget)

All widgets inherit from `panel`.

### Creating Panels

```lisp
(make-instance 'panel
  :x 1 :y 1              ; Position (1-indexed)
  :width 40 :height 10   ; Dimensions
  :border t              ; Show border
  :title "My Panel")     ; Optional title
```

### Panel Accessors

```lisp
(panel-x panel)          ; X position
(panel-y panel)          ; Y position
(panel-width panel)      ; Width
(panel-height panel)     ; Height
(panel-border-p panel)   ; Has border?
(panel-title panel)      ; Title string
(panel-visible-p panel)  ; Is visible?
(panel-active-p panel)   ; Is active/focused?
(panel-dirty-p panel)    ; Needs redraw?
```

### Panel Methods

```lisp
(panel-render panel)     ; Render the panel
(panel-clear panel)      ; Clear panel contents
```

---

## Scrollable List

A list widget with keyboard navigation and scrolling.

### Creating

```lisp
(make-instance 'scrollable-list
  :items '("Item 1" "Item 2" "Item 3")
  :x 1 :y 1 :width 30 :height 10)
```

### Accessors

```lisp
(list-items list)           ; Get/set items
(list-selected-index list)  ; Currently selected index
(list-scroll-offset list)   ; First visible item index
```

### Methods

```lisp
(list-select-next list)     ; Select next item
(list-select-prev list)     ; Select previous item
(list-page-down list)       ; Move down one page
(list-page-up list)         ; Move up one page
(list-selected-item list)   ; Get selected item
(list-handle-key list event) ; Handle key event
```

---

## Text Input

Single-line text input field.

### Creating

```lisp
(make-instance 'text-input
  :width 30
  :value "initial text"
  :placeholder "Enter text..."
  :password t)              ; Show asterisks
```

### Accessors

```lisp
(input-value input)         ; Get/set text value
(input-cursor-pos input)    ; Cursor position
(input-placeholder input)   ; Placeholder text
(input-password-p input)    ; Password mode?
(input-max-length input)    ; Maximum length (nil = unlimited)
```

### Methods

```lisp
(input-insert-char input char)  ; Insert character at cursor
(input-delete-char input)       ; Delete character before cursor
(input-move-left input)         ; Move cursor left
(input-move-right input)        ; Move cursor right
(input-move-to-start input)     ; Move to start
(input-move-to-end input)       ; Move to end
(input-handle-key input event)  ; Handle key event
```

---

## Progress Bar

### Creating

```lisp
(make-instance 'progress-bar
  :x 1 :y 1 :width 40
  :value 0.5                ; 0.0 to 1.0
  :show-percentage t
  :indeterminate nil)       ; Animated spinner mode
```

### Accessors

```lisp
(progress-value bar)        ; Get/set progress (0.0-1.0)
(progress-show-percentage-p bar)
(progress-indeterminate-p bar)
```

### Methods

```lisp
(progress-tick bar)         ; Advance indeterminate animation
```

---

## Status Bar

Horizontal bar with left/center/right sections.

### Creating

```lisp
(make-instance 'status-bar
  :x 1 :y 24 :width 80
  :left "Left text"
  :center "Center"
  :right "Right")
```

### Accessors

```lisp
(status-left bar)
(status-center bar)
(status-right bar)
```

---

## Modal Dialogs

### Alert Dialog

```lisp
(make-alert "Title" "Message text")
```

### Confirm Dialog

```lisp
(make-confirm "Title" "Are you sure?")
```

### Input Prompt

```lisp
(make-input-prompt "Title" "Enter value:")
```

### Dialog Methods

```lisp
(dialog-handle-key dialog event)  ; Handle key
(dialog-result dialog)            ; Get result after close
(dialog-closed-p dialog)          ; Check if closed
```

---

## Menu System

### Menu Items

```lisp
(make-menu-item "Open" :shortcut #\o :value :open)
(make-menu-item "Save" :shortcut #\s :value :save :enabled nil)
(make-separator)              ; Separator line
```

### Popup Menu

```lisp
(make-instance 'menu
  :items (list (make-menu-item "Cut" :value :cut)
               (make-separator)
               (make-menu-item "Paste" :value :paste))
  :x 10 :y 5 :width 20 :height 6)
```

### Menu Methods

```lisp
(menu-select-next menu)
(menu-select-prev menu)
(menu-confirm menu)           ; Select current item
(menu-cancel menu)            ; Close without selection
(menu-handle-key menu event)
(menu-result menu)            ; Get selected value
(menu-closed-p menu)
```

### Menu Bar

```lisp
(make-instance 'menu-bar
  :menus '(("File" . ((make-menu-item "New") (make-menu-item "Open")))
           ("Edit" . ((make-menu-item "Cut") (make-menu-item "Paste"))))
  :x 1 :y 1 :width 80)
```

---

## Table Widget

### Columns

```lisp
(make-column "Name" :name          ; Header and key
  :width 20                        ; Fixed width (nil = auto)
  :align :left                     ; :left, :right, :center
  :formatter #'string-upcase)      ; Optional formatter
```

### Creating Tables

```lisp
(make-instance 'table-widget
  :columns (list (make-column "Name" :name)
                 (make-column "Age" :age :align :right))
  :rows '((:name "Alice" :age 30)
          (:name "Bob" :age 25))
  :sortable t
  :x 1 :y 1 :width 60 :height 20)
```

### Table Methods

```lisp
(table-select-next table)
(table-select-prev table)
(table-page-down table)
(table-page-up table)
(table-select-first table)
(table-select-last table)
(table-sort-by table column-index)
(table-handle-key table event)
(table-selected-data table)       ; Get selected row data
```

---

## Form System

### Fields

```lisp
(make-field "Username" :username
  :required t
  :width 25
  :validator #'(lambda (v) (> (length v) 3)))

(make-field "Password" :password
  :password t
  :required t)
```

### Creating Forms

```lisp
(make-form
  (list (make-field "Name" :name :required t)
        (make-field "Email" :email)
        (make-field "Password" :password :password t))
  :title "Login"
  :label-width 12)
```

### Form Methods

```lisp
(form-focus-next form)
(form-focus-prev form)
(form-submit form)            ; Submit if valid
(form-cancel form)
(form-handle-key form event)
(form-values form)            ; Get values as plist
(form-validate form)          ; Validate all fields
(form-submitted-p form)
(form-cancelled-p form)
```

---

## Tree View

### Tree Nodes

```lisp
(make-node "Root"
  :value :root-data
  :expanded t
  :children (list (make-node "Child 1")
                  (make-node "Child 2"
                    :children (list (make-node "Grandchild")))))
```

### Node Accessors

```lisp
(node-label node)
(node-value node)
(node-children node)
(node-expanded-p node)
(node-parent node)
(node-leaf-p node)
(node-depth node)
```

### Creating Tree Views

```lisp
(make-instance 'tree-view
  :root root-node
  :show-root t               ; Show root node?
  :indent 2                  ; Spaces per level
  :x 1 :y 1 :width 40 :height 20)
```

### Tree Methods

```lisp
(tree-select-next tree)
(tree-select-prev tree)
(tree-expand tree)           ; Expand selected
(tree-collapse tree)         ; Collapse or go to parent
(tree-toggle-expand tree)
(tree-expand-all tree)
(tree-collapse-all tree)
(tree-handle-key tree event)
(tree-selected-node tree)
(tree-selected-value tree)
```

---

## Layout System

### Horizontal/Vertical Layouts

```lisp
(make-instance 'horizontal-layout
  :x 1 :y 1 :width 80 :height 24)

(layout-add layout widget :flex)    ; Flexible size
(layout-add layout widget 20)       ; Fixed 20 units
(layout-add layout widget 0.3)      ; 30% of space
```

### Split Pane

```lisp
(make-instance 'split-pane
  :direction :horizontal           ; or :vertical
  :first panel1
  :second panel2
  :pos 0.5                         ; Split position (0.0-1.0)
  :resizable t
  :min-size 10
  :x 1 :y 1 :width 80 :height 24)
```

---

## Screen Buffer (Double-Buffering)

```lisp
(make-screen-buffer width height)
(buffer-put buffer x y char &key fg bg bold)
(buffer-put-string buffer x y string &key fg bg)
(buffer-clear buffer)
(buffer-render buffer &optional prev-buffer)  ; Diff-based render
```

---

## Resize Handling

```lisp
(install-resize-handler)
(remove-resize-handler)
(resize-pending-p)               ; Check if resize occurred
(clear-resize-flag)              ; Clear the flag
(poll-resize)                    ; Check and return new size if changed
```
