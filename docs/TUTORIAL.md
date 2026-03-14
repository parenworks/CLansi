# Charmed Tutorial

This tutorial walks you through building terminal UI applications with Charmed.

## Installation

```lisp
;; Clone the repository
;; git clone https://github.com/parenworks/Charmed.git ~/common-lisp/charmed

;; Load with Quicklisp (after adding to local-projects or central-registry)
(ql:quickload :charmed)
```

## Hello World

The simplest Charmed program:

```lisp
(defpackage :hello-charmed
  (:use :cl :charmed))
(in-package :hello-charmed)

(defun main ()
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (cursor-to 1 1)
      (bold)
      (fg-color (make-named-color :cyan))
      (format t "Hello, Charmed!")
      (reset)
      (cursor-to 3 1)
      (format t "Press any key to exit...")
      (force-output)
      (read-key))))
```

## Reading Keyboard Input

Charmed provides robust keyboard handling:

```lisp
(defun key-demo ()
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (loop
        (cursor-to 1 1)
        (clear-to-end-of-line)
        (format t "Press a key (q to quit): ")
        (force-output)
        (let ((key (read-key)))
          (cursor-to 3 1)
          (clear-to-end-of-line)
          (cond
            ((key-event-char key)
             (format t "Character: ~A" (key-event-char key))
             (when (char= (key-event-char key) #\q)
               (return)))
            ((key-event-code key)
             (format t "Special key code: ~A" (key-event-code key))))
          (cursor-to 4 1)
          (clear-to-end-of-line)
          (format t "Modifiers: Ctrl=~A Alt=~A Shift=~A"
                  (key-event-ctrl-p key)
                  (key-event-alt-p key)
                  (key-event-shift-p key))
          (force-output))))))
```

## Using Colors

```lisp
(defun color-demo ()
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (cursor-to 1 1)
      
      ;; Named colors
      (dolist (color '(:red :green :blue :yellow :magenta :cyan))
        (fg-color (make-named-color color))
        (format t "~A " color))
      (reset)
      
      ;; RGB colors
      (cursor-to 3 1)
      (loop for r from 0 to 255 by 51
            do (fg-color (make-rgb-color r 0 0))
               (write-char #\█))
      (reset)
      
      ;; Background colors
      (cursor-to 5 1)
      (bg-color (make-named-color :blue))
      (fg-color (make-named-color :white))
      (format t " White on Blue ")
      (reset)
      
      (cursor-to 7 1)
      (format t "Press any key...")
      (force-output)
      (read-key))))
```

## Building a Simple Menu

```lisp
(defun menu-demo ()
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let ((menu (make-instance 'menu
                    :items (list (make-menu-item "New File" :value :new)
                                 (make-menu-item "Open File" :value :open)
                                 (make-separator)
                                 (make-menu-item "Save" :value :save)
                                 (make-menu-item "Save As..." :value :save-as)
                                 (make-separator)
                                 (make-menu-item "Exit" :value :exit))
                    :x 5 :y 3 :width 20 :height 10)))
        (loop until (menu-closed-p menu)
              do (panel-render menu)
                 (menu-handle-key menu (read-key)))
        (clear-screen)
        (cursor-to 1 1)
        (format t "You selected: ~A~%" (menu-result menu))
        (force-output)
        (sleep 1)))))
```

## Creating a Form

```lisp
(defun login-form-demo ()
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let ((form (make-form
                    (list (make-field "Username" :username :required t)
                          (make-field "Password" :password :password t :required t)
                          (make-field "Email" :email))
                    :title "Login")))
        (loop until (or (form-submitted-p form) (form-cancelled-p form))
              do (panel-render form)
                 (form-handle-key form (read-key)))
        (clear-screen)
        (cursor-to 1 1)
        (if (form-submitted-p form)
            (format t "Submitted: ~A~%" (form-values form))
            (format t "Cancelled~%"))
        (force-output)
        (sleep 2)))))
```

## Building a File Browser with Tree View

```lisp
(defun make-directory-tree (path &optional (depth 2))
  "Create a tree node from a directory path."
  (let ((name (or (pathname-name path)
                  (car (last (pathname-directory path)))
                  "/")))
    (if (and (probe-file path)
             (not (pathname-name path))
             (> depth 0))
        (make-node name
          :value path
          :children (loop for child in (directory (merge-pathnames "*.*" path))
                          collect (make-directory-tree child (1- depth))))
        (make-node name :value path))))

(defun file-browser-demo ()
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let* ((root (make-directory-tree (user-homedir-pathname)))
             (tree (make-instance 'tree-view
                     :root root
                     :x 1 :y 1 :width 60 :height 20)))
        (loop
          (panel-render tree)
          (let ((key (read-key)))
            (cond
              ((and (key-event-char key) (char= (key-event-char key) #\q))
               (return))
              ((eql (key-event-code key) +key-enter+)
               ;; Do something with selected file
               (let ((path (tree-selected-value tree)))
                 (cursor-to 22 1)
                 (clear-to-end-of-line)
                 (format t "Selected: ~A" path)
                 (force-output)))
              (t
               (tree-handle-key tree key)))))))))
```

## Data Table Example

```lisp
(defun table-demo ()
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let ((table (make-instance 'table-widget
                     :columns (list (make-column "Name" :name :width 20)
                                    (make-column "Age" :age :width 5 :align :right)
                                    (make-column "City" :city :width 15))
                     :rows '((:name "Alice" :age 30 :city "New York")
                             (:name "Bob" :age 25 :city "Boston")
                             (:name "Charlie" :age 35 :city "Chicago")
                             (:name "Diana" :age 28 :city "Denver")
                             (:name "Eve" :age 32 :city "Seattle"))
                     :sortable t
                     :x 1 :y 1 :width 50 :height 15)))
        (loop
          (panel-render table)
          (let ((key (read-key)))
            (cond
              ((and (key-event-char key) (char= (key-event-char key) #\q))
               (return))
              ((eql (key-event-code key) +key-enter+)
               (cursor-to 17 1)
               (clear-to-end-of-line)
               (format t "Selected: ~A" (table-selected-data table))
               (force-output))
              (t
               (table-handle-key table key)))))))))
```

## Progress Bar Animation

```lisp
(defun progress-demo ()
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let ((bar (make-instance 'progress-bar
                   :x 5 :y 5 :width 50
                   :show-percentage t)))
        (cursor-to 3 5)
        (format t "Processing...")
        (loop for i from 0 to 100
              do (setf (progress-value bar) (/ i 100.0))
                 (panel-render bar)
                 (sleep 0.05))
        (cursor-to 7 5)
        (format t "Done!")
        (force-output)
        (sleep 1)))))
```

## Combining Widgets: A Simple Application

```lisp
(defun todo-app ()
  "A simple TODO list application."
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let* ((items '("Buy groceries" "Write code" "Read book"))
             (list-widget (make-instance 'scrollable-list
                            :items items
                            :x 1 :y 2 :width 40 :height 15))
             (status (make-instance 'status-bar
                       :x 1 :y 20 :width 60
                       :left "TODO List"
                       :right "q:quit a:add d:delete")))
        (loop
          (cursor-to 1 1)
          (bold)
          (format t "My TODO List")
          (reset)
          (panel-render list-widget)
          (panel-render status)
          (let ((key (read-key)))
            (cond
              ((and (key-event-char key) (char= (key-event-char key) #\q))
               (return))
              ((and (key-event-char key) (char= (key-event-char key) #\a))
               ;; Add new item
               (let ((dialog (make-input-prompt "Add TODO" "Enter task:")))
                 (loop until (dialog-closed-p dialog)
                       do (panel-render dialog)
                          (dialog-handle-key dialog (read-key)))
                 (when (dialog-result dialog)
                   (push (dialog-result dialog) items)
                   (setf (list-items list-widget) items))
                 (clear-screen)))
              ((and (key-event-char key) (char= (key-event-char key) #\d))
               ;; Delete selected
               (let ((idx (list-selected-index list-widget)))
                 (setf items (remove (nth idx items) items))
                 (setf (list-items list-widget) items)
                 (clear-screen)))
              (t
               (list-handle-key list-widget key)))))))))
```

## Tips and Best Practices

1. **Always use `with-raw-mode`** - This ensures proper terminal cleanup on exit.

2. **Use `with-alternate-screen`** - Preserves the user's terminal content.

3. **Call `force-output`** - Terminal output is buffered; flush when needed.

4. **Handle Ctrl+C gracefully** - Consider wrapping in `handler-case`.

5. **Check terminal size** - Use `(terminal-size)` to adapt to window size.

6. **Use double-buffering for complex UIs** - Reduces flicker with `screen-buffer`.

```lisp
(defun robust-app ()
  (handler-case
      (with-raw-mode
        (with-alternate-screen
          ;; Your app here
          ))
    (condition (c)
      (format *error-output* "Error: ~A~%" c))))
```
