;;;; file-manager.lisp - Simple file manager example
;;;; Demonstrates tree view and split pane

(defpackage :clansi-file-manager
  (:use :cl :clansi)
  (:export :run))
(in-package :clansi-file-manager)

(defun make-dir-node (path)
  "Create a tree node for a directory."
  (let ((name (or (and (pathname-name path) 
                       (format nil "~A.~A" (pathname-name path) (pathname-type path)))
                  (car (last (pathname-directory path)))
                  (namestring path))))
    (make-node name :value path)))

(defun load-children (node)
  "Load children for a directory node."
  (let ((path (node-value node)))
    (when (and path (probe-file path) (null (pathname-name path)))
      (handler-case
          (let ((children (directory (merge-pathnames "*.*" path))))
            (setf (node-children node)
                  (sort (mapcar #'make-dir-node children)
                        #'string< :key #'node-label))
            (dolist (child (node-children node))
              (setf (node-parent child) node)))
        (error () nil)))))

(defun format-file-info (path)
  "Format file information for display."
  (handler-case
      (let ((size (if (pathname-name path)
                      (with-open-file (s path :if-does-not-exist nil)
                        (if s (file-length s) 0))
                      0)))
        (format nil "~A~%Size: ~:D bytes~%Type: ~A"
                (namestring path)
                size
                (or (pathname-type path) "directory")))
    (error () (format nil "~A~%(unable to read)" (namestring path)))))

(defun run ()
  "Run the file manager."
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let* ((size (terminal-size))
             (width (first size))
             (height (second size))
             (root (make-dir-node (user-homedir-pathname)))
             (tree (make-instance 'tree-view
                     :root root
                     :x 1 :y 2 :width (floor width 2) :height (- height 3)))
             (status (make-instance 'status-bar
                       :x 1 :y height :width width
                       :left "File Manager"
                       :right "q:quit Enter:expand h/l:navigate")))
        ;; Load initial children
        (load-children root)
        (setf (node-expanded-p root) t)
        (tree-compute-visible tree)
        
        (loop
          (clear-screen)
          ;; Header
          (cursor-to 1 1)
          (reverse-video)
          (format t "~VA" width "File Manager")
          (reset)
          
          ;; Tree view
          (panel-render tree)
          
          ;; Info panel
          (let ((info-x (+ 2 (floor width 2)))
                (selected (tree-selected-node tree)))
            (draw-box info-x 2 (- width info-x 1) 10)
            (cursor-to 2 (+ info-x 2))
            (bold)
            (format t " File Info ")
            (reset)
            (when selected
              (let ((info (format-file-info (node-value selected))))
                (loop for line in (split-string info #\Newline)
                      for row from 3
                      do (cursor-to row (1+ info-x))
                         (princ (truncate-string line (- width info-x 3)))))))
          
          (panel-render status)
          (force-output)
          
          (let ((key (read-key)))
            (cond
              ((and (key-event-char key) (char= (key-event-char key) #\q))
               (return))
              ((eql (key-event-code key) +key-enter+)
               ;; Expand and load children
               (let ((node (tree-selected-node tree)))
                 (when (and node (not (node-leaf-p node)))
                   (unless (node-children node)
                     (load-children node))
                   (tree-toggle-expand tree))))
              ((eql (key-event-code key) +key-right+)
               (let ((node (tree-selected-node tree)))
                 (when (and node (not (node-expanded-p node)))
                   (unless (node-children node)
                     (load-children node))))
               (tree-expand tree))
              (t
               (tree-handle-key tree key)))))))))

;;; To run: (clansi-file-manager:run)
