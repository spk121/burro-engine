(define-module (game)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (pb-init)

(define (pb-init server)
  (display "HELLO ")
  (display server)
  (newline)
  0)


;; The protocol detection function: this function is called the 
;; first time a client connects.  The client is expected to
;; identify itself by sending the string "HELLO".
(define (pb-detect-proto server socket)
  (let ((input-bytes (svz:sock:receive-buffer socket))
	(remote-address (svz:sock:remote-address socket)))

    (if (bytevector-contains (svz:sock:receive-buffer socket) 
			     (string->utf8 "HELLO"))
	;; If true, allow connection
	(begin 
	  (format #t "Valid connection from ~a:~a~%" 
		  (inet-ntop AF_INET (htonl (car remote-address)))
		  (cdr remote-address))
	  1)
	;; Otherwise, ignore
	0)))

;; The connect socket function: after a client has been detected by
;; detect-proto, this function handles each request
(define (pb-connect-socket server socket)
  ;; Each packet is going to be delimited by CR/LF
  (svz:sock:boundary socket "\r\n")
  ;; Connect to the request handler
  (svz:sock:handle-request socket pb-handle-request)
  ;; Zero indicates success
  0)

(define (strip-boundary str)
  (string-trim-right str (string->char-set "\r\n")))

;; The request handler.
;; If it receives "HELLO", return "HELLO, WORLD!".
;; If it receives "WHAT TIME IT IS?", return the time.
;; Otherwise, return ERROR.
(define (pb-handle-request socket request len)
  (let ((command (strip-boundary (utf8->string request))))
    (cond
     ((string=? command "HELLO")
      (svz:sock:print socket "HELLO, WORLD!\r\n"))
     ((string=? command "WHAT TIME IS IT?")
      (svz:sock:print socket (strftime "%c" (localtime (current-time))))
      (svz:sock:print socket "\r\n"))
     (else
      (svz:sock:print socket "ERROR\r\n")))
    ;; Return zero to indicate success
    0))

