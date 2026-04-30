;;; terminal.lisp - Raw terminal input/output for Charmed
;;; Handles raw mode, key events, and input parsing

(in-package #:charmed)

;;; ============================================================
;;; Key Event Class
;;; ============================================================

(defclass key-event ()
  ((char :initarg :char :accessor key-event-char :initform nil
         :documentation "Character if printable key")
   (code :initarg :code :accessor key-event-code :initform nil
         :documentation "Keyword for special keys")
   (ctrl-p :initarg :ctrl-p :accessor key-event-ctrl-p :initform nil
           :documentation "Control modifier pressed")
   (alt-p :initarg :alt-p :accessor key-event-alt-p :initform nil
          :documentation "Alt modifier pressed")
   (mouse-x :initarg :mouse-x :accessor key-event-mouse-x :initform nil
            :documentation "Mouse X coordinate (1-indexed)")
   (mouse-y :initarg :mouse-y :accessor key-event-mouse-y :initform nil
            :documentation "Mouse Y coordinate (1-indexed)")
   (mouse-button :initarg :mouse-button :accessor key-event-mouse-button :initform nil
                 :documentation "Mouse button: 0=left, 1=middle, 2=right"))
  (:documentation "Represents a keyboard or mouse input event."))

(defmethod print-object ((key key-event) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~@[char=~S~]~@[ code=~S~]~@[ ctrl~]~@[ alt~]"
            (key-event-char key) (key-event-code key)
            (key-event-ctrl-p key) (key-event-alt-p key))))

(defun make-key-event (&key char code ctrl-p alt-p mouse-x mouse-y mouse-button)
  "Create a key event with the specified attributes."
  (make-instance 'key-event :char char :code code :ctrl-p ctrl-p :alt-p alt-p
                            :mouse-x mouse-x :mouse-y mouse-y
                            :mouse-button mouse-button))

;;; Special key codes
(defconstant +key-up+ :up)
(defconstant +key-down+ :down)
(defconstant +key-left+ :left)
(defconstant +key-right+ :right)
(defconstant +key-enter+ :enter)
(defconstant +key-escape+ :escape)
(defconstant +key-tab+ :tab)
(defconstant +key-backspace+ :backspace)
(defconstant +key-delete+ :delete)
(defconstant +key-home+ :home)
(defconstant +key-end+ :end)
(defconstant +key-page-up+ :page-up)
(defconstant +key-page-down+ :page-down)
(defconstant +key-mouse+ :mouse)
(defconstant +key-mouse-drag+ :mouse-drag)
(defconstant +key-mouse-release+ :mouse-release)
(defconstant +key-resize+ :resize)

;;; Function keys (F1-F12)
(defconstant +key-f1+ :f1)
(defconstant +key-f2+ :f2)
(defconstant +key-f3+ :f3)
(defconstant +key-f4+ :f4)
(defconstant +key-f5+ :f5)
(defconstant +key-f6+ :f6)
(defconstant +key-f7+ :f7)
(defconstant +key-f8+ :f8)
(defconstant +key-f9+ :f9)
(defconstant +key-f10+ :f10)
(defconstant +key-f11+ :f11)
(defconstant +key-f12+ :f12)

;;; ============================================================
;;; Environment Configuration
;;; ============================================================

(defparameter *tty-path*
  (or (getenv "CHARMED_TTY_PATH")
      (let ((candidates '("/dev/tty" "/dev/pts/0" "/dev/console")))
        (loop for path in candidates
              when (ignore-errors (probe-file path))
              return path
              finally (return "/dev/tty"))))
  "Path to the TTY device for input.")

(defparameter *escape-timeout*
  (or (ignore-errors 
        (let ((timeout-str (getenv "CHARMED_ESCAPE_TIMEOUT")))
          (when timeout-str 
            (let ((parsed (read-from-string timeout-str)))
              (if (numberp parsed) parsed nil)))))
      (let ((term (getenv "TERM"))
            (alacritty-socket (getenv "ALACRITTY_SOCKET")))
        (cond
          (alacritty-socket 0.01)
          ((and term (search "alacritty" term)) 0.01)
          (t 0.02))))
  "Seconds to wait for escape sequence bytes.
   Shorter values feel more responsive but may miss sequences on slow connections.")

;;; ============================================================
;;; Terminal Mode Controller Class
;;; ============================================================

(defclass terminal-mode ()
  ((raw-p :initarg :raw-p :accessor terminal-raw-p :initform nil
          :documentation "Whether terminal is currently in raw mode")
   (original-settings :accessor terminal-original-settings :initform nil
                      :documentation "Saved termios for restore")
   (width :accessor terminal-width :initform 80)
   (height :accessor terminal-height :initform 24))
  (:documentation "Manages terminal mode state."))

(defgeneric enable-raw-mode (mode)
  (:documentation "Put terminal in raw mode for character-by-character input."))

(defgeneric disable-raw-mode (mode)
  (:documentation "Restore terminal to normal (cooked) mode."))

(defgeneric query-size (mode)
  (:documentation "Query terminal dimensions, updating width and height slots."))

(defmethod enable-raw-mode ((mode terminal-mode))
  (unless (terminal-raw-p mode)
    (handler-case
        (let* ((fd (%stdin-fd))
               (orig (%get-termios fd))
               (raw (%make-raw-termios orig)))
          (setf (terminal-original-settings mode) orig)
          (when raw
            (%set-termios fd raw)))
      (error (e)
        (warn "Failed to enable raw mode: ~A" e)))
    (setf (terminal-raw-p mode) t)))

(defmethod disable-raw-mode ((mode terminal-mode))
  (when (terminal-raw-p mode)
    (handler-case
        (let ((saved (terminal-original-settings mode)))
          (when saved
            (%set-termios (%stdin-fd) saved)))
      (error (e)
        (warn "Failed to disable raw mode: ~A" e)))
    (setf (terminal-raw-p mode) nil)))

(defmethod query-size ((mode terminal-mode))
  (let ((size (%query-terminal-size (%stdin-fd))))
    (when size
      (setf (terminal-width mode) (first size)
            (terminal-height mode) (second size)))
    size))

;;; Global terminal mode instance
(defparameter *terminal-mode* (make-instance 'terminal-mode)
  "Global terminal mode controller.")

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defun terminal-size ()
  "Return (width height) of terminal."
  (or (query-size *terminal-mode*)
      (list 80 24)))

(defun enable-mouse-tracking ()
  "Enable mouse tracking (SGR extended coordinates).
   Supports click detection and scroll wheel."
  (format *terminal-io* "~C[?1000h~C[?1002h~C[?1006h" 
          *escape* *escape* *escape*)
  (force-output *terminal-io*))

(defun disable-mouse-tracking ()
  "Disable mouse tracking."
  (format *terminal-io* "~C[?1006l~C[?1002l~C[?1000l" 
          *escape* *escape* *escape*)
  (force-output *terminal-io*))

(defmacro with-raw-terminal ((&key (mouse t) (alternate-screen t)) &body body)
  "Execute BODY with terminal in raw mode, ensuring cleanup.
   Options:
     :mouse - Enable mouse tracking (default T)
     :alternate-screen - Use alternate screen buffer (default T)"
  `(progn
     ,(when alternate-screen '(enter-alternate-screen))
     (enable-raw-mode *terminal-mode*)
     ,(when mouse '(enable-mouse-tracking))
     (cursor-hide)
     (unwind-protect
          (progn ,@body)
       (close-tty-stream)
       ,(when mouse '(disable-mouse-tracking))
       (cursor-show)
       (disable-raw-mode *terminal-mode*)
       ,(when alternate-screen '(leave-alternate-screen))
       (reset))))

;;; ============================================================
;;; Input Reader Class
;;; ============================================================

(defclass input-reader ()
  ((stream :initarg :stream :accessor reader-stream :initform nil)
   (tty-path :initarg :tty-path :accessor reader-tty-path :initform *tty-path*))
  (:documentation "Reads and parses keyboard/mouse input from TTY."))

(defmethod print-object ((reader input-reader) stream)
  (print-unreadable-object (reader stream :type t)
    (format stream "~A ~:[closed~;open~]"
            (reader-tty-path reader)
            (and (reader-stream reader) (open-stream-p (reader-stream reader))))))

(defgeneric reader-open (reader)
  (:documentation "Open the TTY stream for reading."))

(defgeneric reader-close (reader)
  (:documentation "Close the TTY stream."))

(defgeneric read-key-event (reader)
  (:documentation "Read a key event from the input stream."))

(defmethod reader-open ((reader input-reader))
  (unless (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (setf (reader-stream reader)
          (open (reader-tty-path reader)
                :direction :input
                :element-type '(unsigned-byte 8)
                :if-does-not-exist :error))
    (%set-nonblocking (%stream-fd (reader-stream reader))))
  reader)

(defmethod reader-close ((reader input-reader))
  (when (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (close (reader-stream reader))
    (setf (reader-stream reader) nil))
  reader)

;;; ============================================================
;;; Input Parsing
;;; ============================================================

(defparameter *read-byte-fn* #'%read-byte-from-fd
  "Function of one argument (fd) that returns a byte or NIL when none is
   ready. Defaults to the real syscall reader. Tests may bind this to
   a closure over a synthetic byte source.")

(declaim (inline %read-byte))
(defun %read-byte (fd)
  "Read one byte through the current *read-byte-fn*."
  (funcall *read-byte-fn* fd))

(defun wait-for-escape-sequence (fd timeout)
  "Wait for escape sequence bytes with timeout.
   Returns the next byte if available within timeout, or NIL."
  (let ((start (get-internal-real-time))
        (timeout-ticks (* timeout internal-time-units-per-second)))
    (loop
      (let ((byte (%read-byte fd)))
        (when byte (return byte)))
      (when (>= (- (get-internal-real-time) start) timeout-ticks)
        (return nil))
      (sleep 0.001))))

(defun read-key-event-from-fd (fd)
  "Read and parse a single key event from FD using the current *read-byte-fn*.

   Returns a KEY-EVENT, or NIL if no input was available. This is the
   transport-independent core of the input dispatcher: it does not touch
   the reader or stream, so it is directly callable from tests that bind
   *read-byte-fn* to a synthetic byte source."
  (let ((byte (%read-byte fd)))
    (unless byte
      (return-from read-key-event-from-fd nil))
    (cond
      ;; Escape sequence or bare escape
      ((= byte 27)
       (let ((next (wait-for-escape-sequence fd *escape-timeout*)))
         (cond
           ((null next)
            (make-key-event :code +key-escape+))
           ((= next 91)  ; CSI: ESC [
            (parse-csi-sequence fd))
           ((= next 79)  ; SS3: ESC O - application cursor mode F1-F4 / arrows
            (parse-ss3-sequence fd))
           (t
            ;; Alt + key
            (make-key-event :char (code-char next) :alt-p t)))))
      ;; Control characters
      ((< byte 32)
       (cond
         ((= byte 13) (make-key-event :code +key-enter+))
         ((= byte 10) (make-key-event :char #\Newline))
         ((= byte 9) (make-key-event :code +key-tab+))
         ((= byte 8) (make-key-event :code +key-backspace+))
         (t (make-key-event :char (code-char (+ byte 96)) :ctrl-p t))))
      ;; DEL character (127) - backspace
      ((= byte 127)
       (make-key-event :code +key-backspace+))
      ;; Regular character (handle UTF-8)
      (t
       (make-key-event :char (decode-utf8-char byte fd))))))

(defmethod read-key-event ((reader input-reader))
  "Read a key event from the TTY."
  (let* ((stream (reader-stream reader))
         (fd (%stream-fd stream)))
    (read-key-event-from-fd fd)))

(defun decode-utf8-char (first-byte fd)
  "Decode a UTF-8 character starting with FIRST-BYTE."
  (cond
    ;; ASCII
    ((< first-byte 128)
     (code-char first-byte))
    ;; 2-byte sequence
    ((and (>= first-byte #xC0) (< first-byte #xE0))
     (let ((b2 (or (%read-byte fd) 0)))
       (code-char (logior (ash (logand first-byte #x1F) 6)
                          (logand b2 #x3F)))))
    ;; 3-byte sequence
    ((and (>= first-byte #xE0) (< first-byte #xF0))
     (let ((b2 (or (%read-byte fd) 0))
           (b3 (or (%read-byte fd) 0)))
       (code-char (logior (ash (logand first-byte #x0F) 12)
                          (ash (logand b2 #x3F) 6)
                          (logand b3 #x3F)))))
    ;; 4-byte sequence
    ((>= first-byte #xF0)
     (let ((b2 (or (%read-byte fd) 0))
           (b3 (or (%read-byte fd) 0))
           (b4 (or (%read-byte fd) 0)))
       (code-char (logior (ash (logand first-byte #x07) 18)
                          (ash (logand b2 #x3F) 12)
                          (ash (logand b3 #x3F) 6)
                          (logand b4 #x3F)))))
    (t (code-char first-byte))))

(defun parse-csi-sequence (fd)
  "Parse a CSI escape sequence after ESC ["
  (let ((first-byte (or (%read-byte fd)
                        (progn (sleep 0.001) (%read-byte fd)))))
    (unless first-byte
      (return-from parse-csi-sequence (make-key-event :code :unknown)))
    (cond
      ;; SGR mouse: ESC [ < ...
      ((= first-byte 60)
       (parse-sgr-mouse fd))
      ;; Normal CSI sequence
      (t
       (parse-normal-csi first-byte fd)))))

(defun parse-sgr-mouse (fd)
  "Parse SGR mouse sequence: ESC [ < Cb ; Cx ; Cy M/m"
  (let ((params nil)
        (final nil))
    (loop for attempts from 0 below 100 do
      (let ((b (%read-byte fd)))
        (cond
          (b (cond
               ((or (and (>= b 48) (<= b 57)) (= b 59))
                (push (code-char b) params))
               (t (setf final b) (return))))
          (t (sleep 0.001)))))
    (let* ((param-str (coerce (nreverse params) 'string))
           (parts (split-string param-str #\;))
           (cb (if (first parts) (parse-integer (first parts) :junk-allowed t) 0))
           (cx (if (second parts) (parse-integer (second parts) :junk-allowed t) 0))
           (cy (if (third parts) (parse-integer (third parts) :junk-allowed t) 0))
           (release-p (and final (= final 109)))
           (button (logand cb 3))
           (motion-p (logbitp 5 cb)))
      (cond
        ;; Scroll up (cb=64)
        ((= cb 64) (make-key-event :code +key-up+))
        ;; Scroll down (cb=65)
        ((= cb 65) (make-key-event :code +key-down+))
        ;; Mouse release
        (release-p
         (make-key-event :code +key-mouse-release+ :mouse-x cx :mouse-y cy
                         :mouse-button button))
        ;; Mouse drag (motion with button held)
        (motion-p
         (make-key-event :code +key-mouse-drag+ :mouse-x cx :mouse-y cy
                         :mouse-button button))
        ;; Mouse press
        ((and final (= final 77))
         (make-key-event :code +key-mouse+ :mouse-x cx :mouse-y cy
                         :mouse-button button))
        (t nil)))))

(defun split-string (string separator)
  "Split STRING by SEPARATOR character."
  (loop for start = 0 then (1+ end)
        for end = (position separator string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun parse-normal-csi (first-byte fd)
  "Parse normal CSI sequence"
  (let ((params (list (code-char first-byte)))
        (final-byte nil))
    ;; If first-byte is already a final byte, use it
    (if (and (>= first-byte 64) (<= first-byte 126))
        (setf final-byte first-byte)
        ;; Otherwise continue reading
        (loop
          (let ((b (%read-byte fd)))
            (unless b (return))
            (cond
              ((and (>= b 48) (<= b 57))
               (push (code-char b) params))
              ((= b 59)
               (push #\; params))
              (t (setf final-byte b) (return))))))
    (let ((param-str (coerce (nreverse params) 'string)))
      (case final-byte
        (65 (make-key-event :code +key-up+))
        (66 (make-key-event :code +key-down+))
        (67 (make-key-event :code +key-right+))
        (68 (make-key-event :code +key-left+))
        (72 (make-key-event :code +key-home+))
        (70 (make-key-event :code +key-end+))
        (126
         ;; Tilde-terminated CSI: editing keys and PC-style F-keys.
         ;; The 16/22 gaps are historical (xterm/VT220 reserved them).
         (cond
           ((string= param-str "3")  (make-key-event :code +key-delete+))
           ((string= param-str "5")  (make-key-event :code +key-page-up+))
           ((string= param-str "6")  (make-key-event :code +key-page-down+))
           ((string= param-str "11") (make-key-event :code +key-f1+))
           ((string= param-str "12") (make-key-event :code +key-f2+))
           ((string= param-str "13") (make-key-event :code +key-f3+))
           ((string= param-str "14") (make-key-event :code +key-f4+))
           ((string= param-str "15") (make-key-event :code +key-f5+))
           ((string= param-str "17") (make-key-event :code +key-f6+))
           ((string= param-str "18") (make-key-event :code +key-f7+))
           ((string= param-str "19") (make-key-event :code +key-f8+))
           ((string= param-str "20") (make-key-event :code +key-f9+))
           ((string= param-str "21") (make-key-event :code +key-f10+))
           ((string= param-str "23") (make-key-event :code +key-f11+))
           ((string= param-str "24") (make-key-event :code +key-f12+))
           (t (make-key-event :code :unknown))))
        (t (make-key-event :code :unknown))))))

(defun parse-ss3-sequence (fd)
  "Parse an SS3 escape sequence after ESC O.

   xterm 'application cursor mode' encodes F1-F4 and the cursor cluster as
   ESC O <letter>. Many terminals send this form for F1-F4 even when not in
   application mode, so the reader has to recognise both encodings.

   Mapping:
     P/Q/R/S    -> F1/F2/F3/F4
     A/B/C/D    -> Up/Down/Right/Left
     H/F        -> Home/End
     anything else, or no second byte within *escape-timeout* -> :unknown."
  (let ((next (wait-for-escape-sequence fd *escape-timeout*)))
    (cond
      ((null next) (make-key-event :code :unknown))
      ((= next 80) (make-key-event :code +key-f1+))
      ((= next 81) (make-key-event :code +key-f2+))
      ((= next 82) (make-key-event :code +key-f3+))
      ((= next 83) (make-key-event :code +key-f4+))
      ((= next 65) (make-key-event :code +key-up+))
      ((= next 66) (make-key-event :code +key-down+))
      ((= next 67) (make-key-event :code +key-right+))
      ((= next 68) (make-key-event :code +key-left+))
      ((= next 72) (make-key-event :code +key-home+))
      ((= next 70) (make-key-event :code +key-end+))
      (t (make-key-event :code :unknown)))))

;;; Global input reader instance
(defparameter *input-reader* (make-instance 'input-reader)
  "Global input reader for keyboard/mouse events.")

(defun close-tty-stream ()
  "Close the TTY stream."
  (reader-close *input-reader*))

(defun read-key ()
  "Read a key event from terminal. Blocks until key pressed."
  (reader-open *input-reader*)
  (loop
    (let ((key (read-key-event *input-reader*)))
      (when key (return key)))
    (sleep 0.01)))

(defun read-key-with-timeout (timeout-ms)
  "Try to read a key event with timeout in milliseconds.
   Returns key-event or NIL if timeout."
  (reader-open *input-reader*)
  (let ((start-time (get-internal-real-time))
        (timeout-ticks (* timeout-ms (/ internal-time-units-per-second 1000))))
    (loop
      (let ((key (read-key-event *input-reader*)))
        (when key (return key)))
      (when (> (- (get-internal-real-time) start-time) timeout-ticks)
        (return nil))
      (sleep 0.01))))

;;; ============================================================
;;; Terminal Resize Handling
;;; ============================================================

(defvar *resize-pending* nil
  "Set to T when a resize event is pending.")

(defvar *last-width* nil
  "Last known terminal width.")

(defvar *last-height* nil
  "Last known terminal height.")

(defun on-resize (width height)
  "Default resize handler - sets pending flag and stores dimensions."
  (setf *last-width* width
        *last-height* height
        *resize-pending* t))

(defun enable-resize-handling (&optional (handler #'on-resize))
  "Enable terminal resize signal handling.
   HANDLER is called with (width height) on resize.
   Default handler sets *resize-pending* and stores dimensions."
  (setf *resize-hook* handler)
  (%install-sigwinch-handler))

(defun disable-resize-handling ()
  "Disable terminal resize signal handling."
  (%remove-sigwinch-handler)
  (setf *resize-hook* nil))

(defun check-resize ()
  "Check if terminal was resized. Returns (values width height) or NIL.
   Clears the pending flag."
  (when *resize-pending*
    (setf *resize-pending* nil)
    (values *last-width* *last-height*)))

(defun poll-resize ()
  "Poll for resize and return key-event if resized, NIL otherwise."
  (multiple-value-bind (width height) (check-resize)
    (when width
      (make-key-event :code +key-resize+ :mouse-x width :mouse-y height))))
