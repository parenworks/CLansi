# Charmed

A lightweight, pure-Lisp ANSI terminal control library. No ncurses dependency.

## Features

- **Pure Lisp** - No FFI dependencies on ncurses or other C libraries
- **ANSI escape sequences** - Colors (256 and 24-bit RGB), cursor control, screen management
- **Raw terminal mode** - Character-by-character input with proper cleanup
- **Key event parsing** - Arrow keys, function keys, mouse events, UTF-8
- **Widget system** - Base panel class and scrollable list for building TUIs
- **Portable** - Full support for SBCL, CCL, and ECL on Linux

## Installation

Clone to your ASDF source registry or Quicklisp local-projects:

```bash
git clone https://github.com/parenworks/Charmed.git ~/quicklisp/local-projects/Charmed
```

Then load:

```lisp
(ql:quickload :charmed)
```

## Quick Start

### Basic Terminal Output

```lisp
(use-package :charmed)

;; Colors
(fg :red)
(princ "Red text")
(reset)

;; RGB colors
(fg-rgb 255 128 0)
(princ "Orange text")
(reset)

;; Styled output
(with-style (:fg :cyan :bold t)
  (princ "Bold cyan text"))

;; Cursor positioning
(cursor-to 10 5)
(princ "At row 10, column 5")

;; Draw a box
(draw-box 1 1 40 10)
```

### Interactive TUI

```lisp
(use-package :charmed)

(defun simple-app ()
  (with-raw-terminal ()
    (clear-screen)
    (cursor-to 1 1)
    (princ "Press 'q' to quit, arrow keys to move")
    (let ((x 20) (y 10))
      (loop
        (cursor-to y x)
        (princ "●")
        (force-output *terminal-io*)
        (let ((key (read-key)))
          (cursor-to y x)
          (princ " ")
          (cond
            ((eql (key-event-char key) #\q) (return))
            ((eql (key-event-code key) +key-up+) (decf y))
            ((eql (key-event-code key) +key-down+) (incf y))
            ((eql (key-event-code key) +key-left+) (decf x))
            ((eql (key-event-code key) +key-right+) (incf x))))))))

(simple-app)
```

### Using Panels

```lisp
(use-package :charmed)

(defun panel-demo ()
  (with-raw-terminal ()
    (let ((list (make-instance 'scrollable-list
                               :x 1 :y 1
                               :width 40 :height 15
                               :title "My List"
                               :items '("Item 1" "Item 2" "Item 3" "Item 4")
                               :active t)))
      (loop
        (clear-screen)
        (panel-render list)
        (force-output *terminal-io*)
        (let ((key (read-key)))
          (cond
            ((eql (key-event-char key) #\q) (return))
            ((eql (key-event-code key) +key-up+)
             (list-move-cursor list -1))
            ((eql (key-event-code key) +key-down+)
             (list-move-cursor list 1))
            ((eql (key-event-code key) +key-enter+)
             (let ((item (list-selected-item list)))
               (cursor-to 20 1)
               (format t "Selected: ~A" item)
               (force-output *terminal-io*)
               (sleep 1)))))))))
```

## API Reference

### Colors

- `(fg color)` - Set foreground color (keyword, index 0-255, or color object)
- `(bg color)` - Set background color
- `(fg-rgb r g b)` - Set foreground to 24-bit RGB
- `(bg-rgb r g b)` - Set background to 24-bit RGB
- `(reset)` - Reset all attributes
- `(bold)`, `(dim)`, `(italic)`, `(underline)`, `(inverse)` - Text styles

### Cursor

- `(cursor-to row col)` - Move cursor (1-indexed)
- `(cursor-home)` - Move to (1, 1)
- `(cursor-up n)`, `(cursor-down n)`, `(cursor-forward n)`, `(cursor-back n)`
- `(cursor-hide)`, `(cursor-show)`

### Screen

- `(clear-screen)` - Clear entire screen
- `(clear-line)` - Clear current line
- `(clear-to-eol)` - Clear to end of line
- `(enter-alternate-screen)`, `(leave-alternate-screen)`
- `(begin-sync-update)`, `(end-sync-update)` - Reduce flicker

### Drawing

- `(draw-box x y width height &key style tl tr bl br h v)`
- `(fill-rect x y width height &optional char)`
- `(write-at row col text &key style)`

### Input

- `(read-key)` - Block until key pressed, return `key-event`
- `(read-key-with-timeout ms)` - Return `key-event` or NIL
- `(terminal-size)` - Return `(width height)`

### Key Event

- `(key-event-char key)` - Character for printable keys
- `(key-event-code key)` - Keyword for special keys (+key-up+, +key-enter+, etc.)
- `(key-event-ctrl-p key)` - Control modifier
- `(key-event-alt-p key)` - Alt modifier
- `(key-event-mouse-x key)`, `(key-event-mouse-y key)` - Mouse coordinates

Special-key constants include `+key-up+`, `+key-down+`, `+key-left+`,
`+key-right+`, `+key-enter+`, `+key-escape+`, `+key-tab+`,
`+key-backspace+`, `+key-delete+`, `+key-home+`, `+key-end+`,
`+key-page-up+`, `+key-page-down+`, and `+key-f1+` through `+key-f12+`.
Function keys are accepted in both the CSI tilde encoding
(`ESC [ N ~`) and the SS3 application-mode encoding
(`ESC O P/Q/R/S` for F1-F4); the cursor cluster is also accepted in
either encoding.

### Widgets

- `panel` - Base class with border, title, position
- `scrollable-list` - List with cursor and scrolling

## Environment Variables

- `CHARMED_TTY_PATH` - Override TTY device path
- `CHARMED_ESCAPE_TIMEOUT` - Escape sequence timeout in seconds (default: 0.02)

## Supported Terminals

Tested with:

- Alacritty
- Kitty
- GNOME Terminal / VTE-based
- iTerm2
- Windows Terminal
- xterm

## Bug Reports & Contributing

Please report bugs and feature requests via GitHub Issues:

https://github.com/parenworks/Charmed/issues

Pull requests are welcome!

## License

MIT

## Credits

Extracted from terminal UI code developed for:

- [CLatter](https://github.com/glenneth1/CLatter) - IRC client
- [CLabber](https://github.com/glenneth1/CLabber) - XMPP client  
- [gilt](https://github.com/glenneth1/gilt) - Git TUI
- [playlisp](https://github.com/fade/playlisp) - Playlist editor
