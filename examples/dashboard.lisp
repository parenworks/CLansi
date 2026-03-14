;;;; dashboard.lisp - Example dashboard application
;;;; Demonstrates combining multiple widgets

(defpackage :charmed-dashboard
  (:use :cl :charmed)
  (:export :run))
(in-package :charmed-dashboard)

(defun make-sample-data ()
  "Generate sample data for the dashboard."
  '((:name "Server 1" :cpu 45 :mem 62 :status "OK")
    (:name "Server 2" :cpu 78 :mem 85 :status "WARN")
    (:name "Server 3" :cpu 23 :mem 41 :status "OK")
    (:name "Database" :cpu 92 :mem 88 :status "CRIT")
    (:name "Cache" :cpu 12 :mem 35 :status "OK")))

(defun status-color (status)
  "Return color for status."
  (cond
    ((string= status "OK") (make-named-color :green))
    ((string= status "WARN") (make-named-color :yellow))
    ((string= status "CRIT") (make-named-color :red))
    (t (make-named-color :white))))

(defun draw-header (width)
  "Draw dashboard header."
  (cursor-to 1 1)
  (bg-color (make-named-color :blue))
  (fg-color (make-named-color :white))
  (bold)
  (let ((title "System Dashboard"))
    (format t "~A~A" 
            (make-string (floor (- width (length title)) 2) :initial-element #\Space)
            title)
    (format t "~A" (make-string (ceiling (- width (length title)) 2) :initial-element #\Space)))
  (reset))

(defun draw-server-panel (data x y width height)
  "Draw server status panel."
  (draw-box x y width height)
  (cursor-to y (+ x 2))
  (bold)
  (format t " Servers ")
  (reset)
  (loop for server in data
        for row from (1+ y)
        do (cursor-to row (1+ x))
           (let ((name (getf server :name))
                 (cpu (getf server :cpu))
                 (status (getf server :status)))
             (format t "~12A CPU:~3D% " name cpu)
             (fg-color (status-color status))
             (format t "~A" status)
             (reset))))

(defun run ()
  "Run the dashboard demo."
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let* ((size (terminal-size))
             (width (first size))
             (height (second size))
             (data (make-sample-data))
             (progress (make-instance 'progress-bar
                         :x 2 :y (- height 4) :width (- width 4)
                         :show-percentage t))
             (status (make-instance 'status-bar
                       :x 1 :y height :width width
                       :left "Dashboard v1.0"
                       :center (format nil "~A servers" (length data))
                       :right "q:quit r:refresh")))
        (loop
          (clear-screen)
          (draw-header width)
          (draw-server-panel data 2 3 40 (+ 2 (length data)))
          
          ;; Simulated load indicator
          (setf (progress-value progress) (/ (random 100) 100.0))
          (cursor-to (- height 5) 2)
          (format t "System Load:")
          (panel-render progress)
          (panel-render status)
          (force-output)
          
          (let ((key (read-key-with-timeout 1000)))
            (when key
              (cond
                ((and (key-event-char key) (char= (key-event-char key) #\q))
                 (return))
                ((and (key-event-char key) (char= (key-event-char key) #\r))
                 ;; Refresh data
                 (setf data (make-sample-data)))))))))))

;;; To run: (charmed-dashboard:run)
