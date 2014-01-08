#!/usr/local/bin/guile \
-s
!#

(use-modules (ice-9 rdelim)
	     (rnrs bytevectors))

(define MAX_MESSAGE_LENGTH 256)
(define Socket (socket PF_INET 
		       SOCK_STREAM 
		       (protoent:proto (getprotobyname "tcp"))))
(define Message (make-bytevector MAX_MESSAGE_LENGTH 0))

(connect Socket AF_INET INADDR_LOOPBACK 50002)
(send Socket (string->utf8 "XBURRO\r\n")) 


(while #t
       (recv! Socket Message)
       (format #t "recv> ~a~%" (utf8->string Message))
       (format #t "send> ")
       (send Socket
	     (string->utf8 (string-append (read-line (current-input-port))
					  "\r\n")))
       (newline))
