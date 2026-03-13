;;; compat.lisp - Portability layer for CLansi
;;; Abstracts implementation-specific features for SBCL, CCL, ECL, etc.

(in-package #:clansi)

;;; ============================================================
;;; Feature Detection
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Ensure trivial-features has set up our feature flags
  #+(or sbcl ccl ecl clasp abcl clisp allegro lispworks)
  nil)

;;; ============================================================
;;; Environment Access
;;; ============================================================

(defun getenv (name)
  "Get environment variable value portably."
  #+sbcl (sb-ext:posix-getenv name)
  #+ccl (ccl:getenv name)
  #+ecl (si:getenv name)
  #+clasp (ext:getenv name)
  #+abcl (ext:getenv name)
  #+clisp (ext:getenv name)
  #+allegro (sys:getenv name)
  #+lispworks (lispworks:environment-variable name)
  #-(or sbcl ccl ecl clasp abcl clisp allegro lispworks)
  (error "GETENV not implemented for this Lisp implementation"))

;;; ============================================================
;;; POSIX Terminal Control
;;; ============================================================

#+sbcl
(progn
  (require :sb-posix)
  
  (defun %get-termios (fd)
    "Get termios structure for file descriptor."
    (sb-posix:tcgetattr fd))
  
  (defun %set-termios (fd termios)
    "Set termios structure for file descriptor."
    (sb-posix:tcsetattr fd sb-posix:tcsaflush termios))
  
  (defun %make-raw-termios (termios)
    "Modify termios for raw mode, return modified copy."
    (let ((raw (sb-posix:tcgetattr (sb-sys:fd-stream-fd sb-sys:*stdin*))))
      ;; Copy settings from original
      (setf (sb-posix:termios-iflag raw)
            (logand (sb-posix:termios-iflag termios)
                    (lognot (logior sb-posix:brkint sb-posix:icrnl
                                   sb-posix:inpck sb-posix:istrip sb-posix:ixon))))
      (setf (sb-posix:termios-oflag raw)
            (logand (sb-posix:termios-oflag termios)
                    (lognot sb-posix:opost)))
      (setf (sb-posix:termios-cflag raw)
            (logior (sb-posix:termios-cflag termios) sb-posix:cs8))
      (setf (sb-posix:termios-lflag raw)
            (logand (sb-posix:termios-lflag termios)
                    (lognot (logior sb-posix:echo sb-posix:icanon
                                   sb-posix:iexten sb-posix:isig))))
      ;; VMIN=1 VTIME=0
      (let ((cc (sb-posix:termios-cc raw)))
        (setf (aref cc sb-posix:vmin) 1)
        (setf (aref cc sb-posix:vtime) 0))
      raw))
  
  (defun %stdin-fd ()
    "Get file descriptor for stdin."
    (sb-sys:fd-stream-fd sb-sys:*stdin*))
  
  (defun %stream-fd (stream)
    "Get file descriptor for a stream."
    (sb-sys:fd-stream-fd stream))
  
  (defun %set-nonblocking (fd)
    "Set file descriptor to non-blocking mode."
    (sb-posix:fcntl fd sb-posix:f-setfl
                    (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                            sb-posix:o-nonblock)))
  
  (defun %read-byte-from-fd (fd)
    "Read a single byte from file descriptor. Returns byte or NIL."
    (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
      (declare (dynamic-extent buf))
      (let ((n (sb-unix:unix-read fd (sb-sys:vector-sap buf) 1)))
        (when (and n (= n 1))
          (aref buf 0)))))
  
  (defun %query-terminal-size (fd)
    "Query terminal size via ioctl. Returns (width height) or NIL."
    (handler-case
        (sb-alien:with-alien ((buf (sb-alien:array (sb-alien:unsigned 8) 8)))
          (sb-alien:alien-funcall
           (sb-alien:extern-alien "ioctl"
                                  (function sb-alien:int sb-alien:int
                                            sb-alien:unsigned-long (* t)))
           fd
           #x5413  ; TIOCGWINSZ
           (sb-alien:addr (sb-alien:deref buf 0)))
          (let ((rows (logior (sb-alien:deref buf 0) (ash (sb-alien:deref buf 1) 8)))
                (cols (logior (sb-alien:deref buf 2) (ash (sb-alien:deref buf 3) 8))))
            (when (and (> rows 0) (> cols 0))
              (list cols rows))))
      (error () nil))))

#+ccl
(progn
  (defun %get-termios (fd)
    "Get termios structure for file descriptor."
    (ccl:rlet ((termios :termios))
      (ccl:external-call "tcgetattr" :int fd :address termios :int)
      (ccl:%copy-ivector-to-ivector termios 0 
                                     (make-array 60 :element-type '(unsigned-byte 8)) 0 60)))
  
  (defun %set-termios (fd termios)
    "Set termios structure for file descriptor."
    ;; CCL implementation - simplified
    (declare (ignore fd termios))
    (warn "CCL termios support is limited"))
  
  (defun %make-raw-termios (termios)
    "Modify termios for raw mode."
    (declare (ignore termios))
    (warn "CCL raw mode support is limited")
    nil)
  
  (defun %stdin-fd ()
    "Get file descriptor for stdin."
    0)  ; stdin is always fd 0
  
  (defun %stream-fd (stream)
    "Get file descriptor for a stream."
    (ccl:stream-device stream :input))
  
  (defun %set-nonblocking (fd)
    "Set file descriptor to non-blocking mode."
    (ccl:external-call "fcntl" :int fd :int 4 :int 2048 :int))  ; F_SETFL, O_NONBLOCK
  
  (defun %read-byte-from-fd (fd)
    "Read a single byte from file descriptor."
    (ccl:rlet ((buf (:array :unsigned-byte 1)))
      (let ((n (ccl:external-call "read" :int fd :address buf :size-t 1 :ssize-t)))
        (when (= n 1)
          (ccl:%get-unsigned-byte buf 0)))))
  
  (defun %query-terminal-size (fd)
    "Query terminal size."
    (declare (ignore fd))
    ;; Fallback - try environment or default
    (let ((cols (ignore-errors (parse-integer (getenv "COLUMNS"))))
          (rows (ignore-errors (parse-integer (getenv "LINES")))))
      (list (or cols 80) (or rows 24)))))

#+ecl
(progn
  (defun %get-termios (fd)
    (declare (ignore fd))
    (warn "ECL termios support not yet implemented")
    nil)
  
  (defun %set-termios (fd termios)
    (declare (ignore fd termios))
    nil)
  
  (defun %make-raw-termios (termios)
    (declare (ignore termios))
    nil)
  
  (defun %stdin-fd ()
    0)
  
  (defun %stream-fd (stream)
    (declare (ignore stream))
    0)
  
  (defun %set-nonblocking (fd)
    (declare (ignore fd))
    nil)
  
  (defun %read-byte-from-fd (fd)
    (declare (ignore fd))
    nil)
  
  (defun %query-terminal-size (fd)
    (declare (ignore fd))
    (list 80 24)))

#-(or sbcl ccl ecl)
(progn
  (defun %get-termios (fd)
    (declare (ignore fd))
    (error "Terminal control not implemented for this Lisp"))
  
  (defun %set-termios (fd termios)
    (declare (ignore fd termios))
    (error "Terminal control not implemented for this Lisp"))
  
  (defun %make-raw-termios (termios)
    (declare (ignore termios))
    (error "Terminal control not implemented for this Lisp"))
  
  (defun %stdin-fd ()
    (error "Terminal control not implemented for this Lisp"))
  
  (defun %stream-fd (stream)
    (declare (ignore stream))
    (error "Terminal control not implemented for this Lisp"))
  
  (defun %set-nonblocking (fd)
    (declare (ignore fd))
    (error "Terminal control not implemented for this Lisp"))
  
  (defun %read-byte-from-fd (fd)
    (declare (ignore fd))
    (error "Terminal control not implemented for this Lisp"))
  
  (defun %query-terminal-size (fd)
    (declare (ignore fd))
    (list 80 24)))
