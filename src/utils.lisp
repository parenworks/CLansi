;;; utils.lisp - System integration utilities for CLansi
;;; URL detection, clipboard operations, and browser launching

(in-package #:clansi)

;;; ============================================================
;;; URL Detection
;;; ============================================================

(defun find-urls (text)
  "Find URLs in TEXT. Returns list of (start end url) triples."
  (let ((results nil)
        (pos 0)
        (len (length text)))
    (loop
      (let ((http-pos (search "http" text :start2 pos)))
        (unless http-pos (return (nreverse results)))
        ;; Check it's http:// or https://
        (let ((remaining (- len http-pos)))
          (if (or (and (>= remaining 8)
                       (string= "https://" (subseq text http-pos (+ http-pos 8))))
                  (and (>= remaining 7)
                       (string= "http://" (subseq text http-pos (+ http-pos 7)))))
              ;; Valid URL - find its end
              (let ((end (or (position-if (lambda (c)
                                            (member c '(#\Space #\Tab #\Newline #\Return
                                                        #\> #\< #\) #\])))
                                          text :start http-pos)
                             len)))
                ;; Strip trailing punctuation
                (loop while (and (> end http-pos)
                                (member (char text (1- end)) '(#\. #\, #\; #\: #\! #\?)))
                      do (decf end))
                (push (list http-pos end (subseq text http-pos end)) results)
                (setf pos end))
              ;; Not a URL, skip past this "http"
              (setf pos (1+ http-pos))))))))

(defun princ-with-urls (text &optional (stream *terminal-io*))
  "Print TEXT to STREAM, wrapping URLs in OSC 8 hyperlinks with underline."
  (let ((urls (find-urls text))
        (pos 0))
    (if (null urls)
        (princ text stream)
        (progn
          (dolist (url-info urls)
            (destructuring-bind (start end url) url-info
              ;; Print text before URL
              (when (> start pos)
                (princ (subseq text pos start) stream))
              ;; Print URL as clickable hyperlink
              (begin-hyperlink url)
              (underline)
              (princ url stream)
              (end-hyperlink)
              (reset)
              (setf pos end)))
          ;; Print remaining text after last URL
          (when (< pos (length text))
            (princ (subseq text pos) stream))))))

;;; ============================================================
;;; Clipboard Operations
;;; ============================================================

(defun copy-to-clipboard (text)
  "Copy TEXT to system clipboard. Tries wl-copy (Wayland), xclip, xsel."
  (handler-case
      (let ((cmd (cond
                   ((probe-file "/usr/bin/wl-copy") "wl-copy")
                   ((probe-file "/usr/bin/xclip") "xclip -selection clipboard")
                   ((probe-file "/usr/bin/xsel") "xsel --clipboard --input")
                   (t nil))))
        (when cmd
          (let ((proc (sb-ext:run-program "/bin/sh" (list "-c" cmd)
                                          :input :stream :wait nil)))
            (write-string text (sb-ext:process-input proc))
            (close (sb-ext:process-input proc))
            (sb-ext:process-wait proc)
            t)))
    (error () nil)))

(defun paste-from-clipboard ()
  "Get text from system clipboard. Tries wl-paste (Wayland), xclip, xsel."
  (handler-case
      (let ((cmd (cond
                   ((probe-file "/usr/bin/wl-paste") "wl-paste -n")
                   ((probe-file "/usr/bin/xclip") "xclip -selection clipboard -o")
                   ((probe-file "/usr/bin/xsel") "xsel --clipboard --output")
                   (t nil))))
        (when cmd
          (let ((proc (sb-ext:run-program "/bin/sh" (list "-c" cmd)
                                          :output :stream :wait nil)))
            (prog1 (read-line (sb-ext:process-output proc) nil "")
              (sb-ext:process-wait proc)))))
    (error () nil)))

;;; ============================================================
;;; URL Opening
;;; ============================================================

(defun open-url (url)
  "Open URL in the default browser/handler.
   Uses xdg-open on Linux, open on macOS."
  (handler-case
      (let ((cmd #+darwin "/usr/bin/open"
                 #-darwin "/usr/bin/xdg-open"))
        (sb-ext:run-program cmd (list url)
                            :wait nil :input nil :output nil)
        t)
    (error () nil)))
