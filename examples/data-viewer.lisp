;;;; data-viewer.lisp - Data table viewer example
;;;; Demonstrates table widget with sorting

(defpackage :charmed-data-viewer
  (:use :cl :charmed)
  (:export :run))
(in-package :charmed-data-viewer)

(defun generate-sample-data (n)
  "Generate N rows of sample data."
  (let ((cities '("New York" "Los Angeles" "Chicago" "Houston" "Phoenix"
                  "Philadelphia" "San Antonio" "San Diego" "Dallas" "Austin"))
        (departments '("Engineering" "Sales" "Marketing" "HR" "Finance")))
    (loop for i from 1 to n
          collect (list :id i
                        :name (format nil "Employee ~D" i)
                        :department (nth (random (length departments)) departments)
                        :city (nth (random (length cities)) cities)
                        :salary (+ 40000 (random 80000))))))

(defun format-salary (value)
  "Format salary with currency."
  (format nil "$~:D" value))

(defun run ()
  "Run the data viewer."
  (with-raw-mode
    (with-alternate-screen
      (clear-screen)
      (let* ((size (terminal-size))
             (width (first size))
             (height (second size))
             (data (generate-sample-data 50))
             (table (make-instance 'table-widget
                      :columns (list (make-column "ID" :id :width 5 :align :right)
                                     (make-column "Name" :name :width 15)
                                     (make-column "Department" :department :width 12)
                                     (make-column "City" :city :width 15)
                                     (make-column "Salary" :salary :width 12 
                                                  :align :right :formatter #'format-salary))
                      :rows data
                      :sortable t
                      :x 1 :y 2 :width (- width 2) :height (- height 4)))
             (status (make-instance 'status-bar
                       :x 1 :y height :width width
                       :left (format nil "~D records" (length data))
                       :center "1-5: sort by column"
                       :right "q:quit j/k:navigate")))
        (loop
          (clear-screen)
          ;; Header
          (cursor-to 1 1)
          (bold)
          (fg-color (make-named-color :cyan))
          (format t "Employee Database Viewer")
          (reset)
          
          ;; Table
          (panel-render table)
          
          ;; Selected row info
          (let ((selected (table-selected-data table)))
            (when selected
              (setf (status-left status)
                    (format nil "Selected: ~A (~A)"
                            (getf selected :name)
                            (getf selected :department)))))
          
          (panel-render status)
          (force-output)
          
          (let ((key (read-key)))
            (cond
              ((and (key-event-char key) (char= (key-event-char key) #\q))
               (return))
              (t
               (table-handle-key table key)))))))))

;;; To run: (charmed-data-viewer:run)
