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
  ;; CCL termios structure layout (Linux x86-64)
  ;; struct termios { tcflag_t c_iflag, c_oflag, c_cflag, c_lflag; cc_t c_cc[32]; ... }
  ;; tcflag_t is 4 bytes, cc_t is 1 byte
  
  (defconstant +termios-size+ 60)
  (defconstant +cc-offset+ 16)  ; offset to c_cc array
  (defconstant +vmin+ 6)
  (defconstant +vtime+ 5)
  
  ;; termios flag bits (Linux)
  (defconstant +brkint+ #o0000002)
  (defconstant +icrnl+  #o0000400)
  (defconstant +inpck+  #o0000020)
  (defconstant +istrip+ #o0000040)
  (defconstant +ixon+   #o0002000)
  (defconstant +opost+  #o0000001)
  (defconstant +cs8+    #o0000060)
  (defconstant +echo+   #o0000010)
  (defconstant +icanon+ #o0000002)
  (defconstant +iexten+ #o0100000)
  (defconstant +isig+   #o0000001)
  (defconstant +tcsaflush+ 2)
  
  (defun %get-termios (fd)
    "Get termios structure for file descriptor."
    (let ((termios (make-array +termios-size+ :element-type '(unsigned-byte 8) :initial-element 0)))
      (ccl:with-pointer-to-ivector (ptr termios)
        (ccl:external-call "tcgetattr" :int fd :address ptr :int))
      termios))
  
  (defun %set-termios (fd termios)
    "Set termios structure for file descriptor."
    (ccl:with-pointer-to-ivector (ptr termios)
      (ccl:external-call "tcsetattr" :int fd :int +tcsaflush+ :address ptr :int)))
  
  (defun %get-termios-flag (termios offset)
    "Get a 4-byte flag from termios at byte offset."
    (logior (aref termios offset)
            (ash (aref termios (+ offset 1)) 8)
            (ash (aref termios (+ offset 2)) 16)
            (ash (aref termios (+ offset 3)) 24)))
  
  (defun %set-termios-flag (termios offset value)
    "Set a 4-byte flag in termios at byte offset."
    (setf (aref termios offset) (logand value #xff))
    (setf (aref termios (+ offset 1)) (logand (ash value -8) #xff))
    (setf (aref termios (+ offset 2)) (logand (ash value -16) #xff))
    (setf (aref termios (+ offset 3)) (logand (ash value -24) #xff)))
  
  (defun %make-raw-termios (termios)
    "Modify termios for raw mode, return modified copy."
    (let ((raw (copy-seq termios)))
      ;; c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON)
      (%set-termios-flag raw 0 
                         (logand (%get-termios-flag raw 0)
                                 (lognot (logior +brkint+ +icrnl+ +inpck+ +istrip+ +ixon+))))
      ;; c_oflag &= ~OPOST
      (%set-termios-flag raw 4
                         (logand (%get-termios-flag raw 4) (lognot +opost+)))
      ;; c_cflag |= CS8
      (%set-termios-flag raw 8
                         (logior (%get-termios-flag raw 8) +cs8+))
      ;; c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG)
      (%set-termios-flag raw 12
                         (logand (%get-termios-flag raw 12)
                                 (lognot (logior +echo+ +icanon+ +iexten+ +isig+))))
      ;; c_cc[VMIN] = 1, c_cc[VTIME] = 0
      (setf (aref raw (+ +cc-offset+ +vmin+)) 1)
      (setf (aref raw (+ +cc-offset+ +vtime+)) 0)
      raw))
  
  (defun %stdin-fd ()
    "Get file descriptor for stdin."
    0)
  
  (defun %stream-fd (stream)
    "Get file descriptor for a stream."
    (ccl:stream-device stream :input))
  
  (defun %set-nonblocking (fd)
    "Set file descriptor to non-blocking mode."
    (let ((flags (ccl:external-call "fcntl" :int fd :int 3 :int)))  ; F_GETFL = 3
      (ccl:external-call "fcntl" :int fd :int 4 :int (logior flags 2048) :int)))  ; F_SETFL = 4, O_NONBLOCK = 2048
  
  (defun %read-byte-from-fd (fd)
    "Read a single byte from file descriptor."
    (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
      (ccl:with-pointer-to-ivector (ptr buf)
        (let ((n (ccl:external-call "read" :int fd :address ptr :size-t 1 :ssize-t)))
          (when (and n (= n 1))
            (aref buf 0))))))
  
  (defun %query-terminal-size (fd)
    "Query terminal size via ioctl."
    (let ((winsize (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
      (ccl:with-pointer-to-ivector (ptr winsize)
        (ccl:external-call "ioctl" :int fd :unsigned-long #x5413 :address ptr :int))  ; TIOCGWINSZ
      (let ((rows (logior (aref winsize 0) (ash (aref winsize 1) 8)))
            (cols (logior (aref winsize 2) (ash (aref winsize 3) 8))))
        (if (and (> rows 0) (> cols 0))
            (list cols rows)
            (list 80 24))))))

#+ecl
(progn
  ;; ECL termios structure layout (Linux x86-64)
  (defconstant +termios-size+ 60)
  (defconstant +cc-offset+ 16)
  (defconstant +vmin+ 6)
  (defconstant +vtime+ 5)
  
  ;; termios flag bits (Linux)
  (defconstant +brkint+ #o0000002)
  (defconstant +icrnl+  #o0000400)
  (defconstant +inpck+  #o0000020)
  (defconstant +istrip+ #o0000040)
  (defconstant +ixon+   #o0002000)
  (defconstant +opost+  #o0000001)
  (defconstant +cs8+    #o0000060)
  (defconstant +echo+   #o0000010)
  (defconstant +icanon+ #o0000002)
  (defconstant +iexten+ #o0100000)
  (defconstant +isig+   #o0000001)
  (defconstant +tcsaflush+ 2)
  
  (ffi:clines "
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
")
  
  (defun %get-termios (fd)
    "Get termios structure for file descriptor."
    (let ((termios (make-array +termios-size+ :element-type '(unsigned-byte 8) :initial-element 0)))
      (ffi:c-inline (fd termios) (:int :object) :void
        "tcgetattr(#0, (struct termios*)#1->vector.self.b8)")
      termios))
  
  (defun %set-termios (fd termios)
    "Set termios structure for file descriptor."
    (ffi:c-inline (fd termios) (:int :object) :void
      "tcsetattr(#0, TCSAFLUSH, (struct termios*)#1->vector.self.b8)"))
  
  (defun %get-termios-flag (termios offset)
    "Get a 4-byte flag from termios at byte offset."
    (logior (aref termios offset)
            (ash (aref termios (+ offset 1)) 8)
            (ash (aref termios (+ offset 2)) 16)
            (ash (aref termios (+ offset 3)) 24)))
  
  (defun %set-termios-flag (termios offset value)
    "Set a 4-byte flag in termios at byte offset."
    (setf (aref termios offset) (logand value #xff))
    (setf (aref termios (+ offset 1)) (logand (ash value -8) #xff))
    (setf (aref termios (+ offset 2)) (logand (ash value -16) #xff))
    (setf (aref termios (+ offset 3)) (logand (ash value -24) #xff)))
  
  (defun %make-raw-termios (termios)
    "Modify termios for raw mode, return modified copy."
    (let ((raw (copy-seq termios)))
      (%set-termios-flag raw 0 
                         (logand (%get-termios-flag raw 0)
                                 (lognot (logior +brkint+ +icrnl+ +inpck+ +istrip+ +ixon+))))
      (%set-termios-flag raw 4
                         (logand (%get-termios-flag raw 4) (lognot +opost+)))
      (%set-termios-flag raw 8
                         (logior (%get-termios-flag raw 8) +cs8+))
      (%set-termios-flag raw 12
                         (logand (%get-termios-flag raw 12)
                                 (lognot (logior +echo+ +icanon+ +iexten+ +isig+))))
      (setf (aref raw (+ +cc-offset+ +vmin+)) 1)
      (setf (aref raw (+ +cc-offset+ +vtime+)) 0)
      raw))
  
  (defun %stdin-fd ()
    "Get file descriptor for stdin."
    0)
  
  (defun %stream-fd (stream)
    "Get file descriptor for a stream."
    (si:file-stream-fd stream))
  
  (defun %set-nonblocking (fd)
    "Set file descriptor to non-blocking mode."
    (ffi:c-inline (fd) (:int) :void
      "fcntl(#0, F_SETFL, fcntl(#0, F_GETFL) | O_NONBLOCK)"))
  
  (defun %read-byte-from-fd (fd)
    "Read a single byte from file descriptor."
    (let ((result (ffi:c-inline (fd) (:int) :int
                    "{
                       unsigned char buf;
                       int n = read(#0, &buf, 1);
                       @(return) = (n == 1) ? buf : -1;
                     }")))
      (when (>= result 0) result)))
  
  (defun %query-terminal-size (fd)
    "Query terminal size via ioctl."
    (let ((cols 0) (rows 0))
      (ffi:c-inline (fd) (:int) :void
        "{
           struct winsize ws;
           if (ioctl(#0, TIOCGWINSZ, &ws) == 0) {
             @cols = ws.ws_col;
             @rows = ws.ws_row;
           }
         }")
      (if (and (> cols 0) (> rows 0))
          (list cols rows)
          (list 80 24)))))

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
