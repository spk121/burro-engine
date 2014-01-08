(define-module (game)
  #:use-module (serveez-mg core)
  #:use-module (serveez-mg lib)
  #:use-module (rnrs bytevectors)
  #:use-module (repl))

;; === Game App Server Definition =========================================

;; Once a client connects, it has 16 bytes and 30 seconds to prove
;; that is should be joined to this server.  This function makes the
;; decision if this client is acceptable.  `svz:sock:receive-buffer'
;; can be used to peek at the client's initial message sent.  It
;; returns *non-zero* to indicate that this client can be joined to
;; this server.
(define (pb-detect-proto server socket)
  (format #t "(pb-detect-proto ~a ~a)~%" server socket)
  (let ((client-message (svz:sock:receive-buffer socket))
	(client-address (svz:sock:remote-address socket)))
    (if (string= (utf8->string client-message)
		 "BURRO")
	(begin
	  (format #t "Valid connection from ~a:~a~%" 
		  (inet-ntop AF_INET (htonl (car client-address)))
		  (cdr client-address))
	  (format #t "~s~%" (utf8->string client-message))
	  1)
	0)))

;; Called when the server type is defined with `define-servertype!'.
;; Here one sets up resources that are shared among all instances of
;; the game server.  Return 0 on success.
(define (pb-global-init server)
  (format #t "(pb-global-init ~a)~%" server)
  0)

;; Called when define-server! is called.  Initialize resources
;; required for a single instance of the game server.  Return 0 on
;; success.
(define (pb-init server)
  (format #t "(pb-init ~a)~%" server)
  0)

;; Called when an instance of a server is freed.  Free resources set
;; in the initializer.  Return 0 to indicate success.
(define (pb-finalize server)
  (format #t "(pb-finalize ~a)~%" server)
  0)

;; Called when `serveez-nuke' is called.  Free resources set in the 
;; global initializer.  Return 0 on success.
(define (pb-global-finalize server)
  (format #t "(pb-global-finalize ~a)~%" server)
  0)

;; If a client has connected and has been recognized by detect-proto,
;; this callback assigns that client to the instance of the server
;; that claimed it.  Need to assign the `check-request' callback
;; and do any per-client initialization tasks.  Return 0 on success;
;; non-zero to shut down the connection.
(define (pb-connect-socket server socket)
  (format #t "(pb-connect-socket ~a ~a)~%" server socket)

  ;; Start a REPL
  (call-with-new-thread (lambda () (spawn-repl server socket)))
  
  ;; Connect PB message handler to this socket
  (svz:sock:check-request socket pb-check-request)
  0)

;; This callback gets invoked by the Control Protocol Server when
;; the operator requests info.  It returns a string with
;; information about a server.
(define (pb-info-server server)
  (format #t "(pb-info-server ~a)~%" server)
  "PB SERVER")

;; This callback gets called by the Control Protocol Server.  It
;; returns a single-line string about each client attached to a
;; server.
(define (pb-info-client server socket)
  (format #t "(pb-info-client ~a ~a)~%" server socket)
  "PB CLIENT")

;; This is an on-idle function called every second if nothing
;; else is happening.
(define (pb-notify server)
  ;; (format #t "(pb-notify ~a)~%" server)
  ;; What should I return?
  0)

;; This callback gets called when Serveez receives SIGHUP.
(define (pb-reset server)
  (format #t "(pb-reset ~a)~%" server)
  ;; What should I return?
  0)

(define-servertype! 
  `(
    ;; Server type prefix (mandatory)
    (prefix         . "pb")

    ;; Server type description (mandatory)
    (description    . "Project Burro game engine")

    ;; Protocol Detection (mandatory for TCP)
    (detect-proto   . ,pb-detect-proto)

    ;; Global server type initialization (optional)
    (global-init    . ,pb-global-init)

    ;; Server instance initialization (optional)
    (init           . ,pb-init)

    ;; Server instance finalization (optional)
    (finalize       . ,pb-finalize)

    ;; Global server type finalization (optional)
    (global-finalize . ,pb-global-finalize)

    ;; Socket connection (mandatory for TCP)
    (connect-socket  . ,pb-connect-socket)

    ;; Server instance info (optional)
    (info-server     . ,pb-info-server)

    ;; Client info (optional)
    (info-client     . ,pb-info-client)

    ;; Server instance reset callback
    (reset           . ,pb-reset)
    
    ;; Server instance notifier
    (notify          . ,pb-notify)

    ;; Packet handler (mandatory for UDP)
    ;; (handle-request . ,pb-handle-request)

    ;; Server type configuration
    (configuration   . ())))

;; === Client Handler Definition ==========================================

;; Called whenever new data has arrived.  The received buffer
;; can bee peeked with `svz:sock:receive-buffer.  If there is
;; a complete request
;; - call `handle-request' telling it the length of the packet
;; - shrink the receive buffer
;; Return 0 on success or non-zero to shut down the connection.
(define (pb-check-request socket)
  (format #t "(pb-check-request ~a)~%" socket)
  (let ((ret 0))			; zero means good
    (while #t
	   (let* ((data (svz:sock:receive-buffer socket))
		  (index (bytevector-contains data (string->utf8 "\r\n")))
		  (message #f))
	     (if index
		 (begin
		   (set! message (subbytevector data 0 index))
		   (set! ret (pb-handle-request socket message index))
		   (svz:sock:receive-buffer-reduce socket (+ index 2)))
		 ;; else
		 (break))))
    ret))

;; Once `check-request' has certified that there is a complete
;; packet of data, it passes it to this function for processing.
;; It is called by `pb-check-request'.  It returns 0 for success.
;; Non-zero indicates that you want to shut down the connection.
(define (pb-handle-request socket bytevector size)
  (format #t "(pb-handle-request ~a ~a ~a)~%" socket bytevector size)
  (format #t "handling ~s~%" (utf8->string bytevector))
  (svz:sock:print socket (string->utf8 "NOOP\r\n"))
  0)

;; Called when a socket is lost for an external reason.
;; Set with `svz:sock:disconnected'.
(define (pb-disconnected socket)
  ;; What should I return?
  0)

;; Called when serveez detects a flood or a buffer overflow.
;; Set with `svz:sock:kicked'
(define (pb-kicked socket reason)
  ;; What should I return?
  0)

;; Called when the countdown timer drops to zero.
;; Set this callback with `svz:sock:idle'.  Set the 
;; callback timer with `svz:sock:idle-counter'
(define (pb-idle socket)
  0)

;; Called once per server loop.  If it returns #t,
;; the trigger callback will be called.  Set
;; this with `svz:sock:trigger-condition'
(define (pb-trigger-condition socket)
  #f)

;; Called each server loop that pb-trigger-condition
;; returns #t.  Set this with `svz:sock:trigger'.
;; Returns 0 on success or non-zero to shut the
;; connection.
(define (pb-trigger socket)
  0)

;; Port configuration
(define-port! 'pb-port '((proto . tcp)
			 (port . 50002)))

;; Server instantiation
(define-server! 'pb-server '())

(bind-server! 'pb-port 'pb-server)


;; === Debug Console Server Definition =========================================

(define (db-detect-proto server socket)
  (format #t "(db-detect-proto ~a ~a)~%" server socket)
  (let ((client-message (svz:sock:receive-buffer socket))
	(client-address (svz:sock:remote-address socket)))
    (if (string= (utf8->string client-message)
		 "XBURRO\r\n")
	(begin
	  (format #t "Valid connection from ~a:~a~%" 
		  (inet-ntop AF_INET (htonl (car client-address)))
		  (cdr client-address))
	  (format #t "~s~%" (utf8->string client-message))
	  1)
	0)))

;; Called when the server type is defined with `define-servertype!'.
;; Here one sets up resources that are shared among all instances of
;; the game server.  Return 0 on success.
(define (db-global-init server)
  (format #t "(db-global-init ~a)~%" server)
  0)

;; Called when define-server! is called.  Initialize resources
;; required for a single instance of the game server.  Return 0 on
;; success.
(define (db-init server)
  (format #t "(db-init ~a)~%" server)
  0)

;; Called when an instance of a server is freed.  Free resources set
;; in the initializer.  Return 0 to indicate success.
(define (db-finalize server)
  (format #t "(db-finalize ~a)~%" server)
  0)

;; Called when `serveez-nuke' is called.  Free resources set in the 
;; global initializer.  Return 0 on success.
(define (db-global-finalize server)
  (format #t "(db-global-finalize ~a)~%" server)
  0)

;; If a client has connected and has been recognized by detect-proto,
;; this callback assigns that client to the instance of the server
;; that claimed it.  Need to assign the `check-request' callback
;; and do any per-client initialization tasks.  Return 0 on success;
;; non-zero to shut down the connection.
(define (db-connect-socket server socket)
  (format #t "(db-connect-socket ~a ~a)~%" server socket)

  ;; Start a REPL
  (call-with-new-thread (lambda () (spawn-repl server socket)))
  
  ;; Connect PB message handler to this socket
  (svz:sock:check-request socket db-check-request)
  0)

;; This callback gets invoked by the Control Protocol Server when
;; the operator requests info.  It returns a string with
;; information about a server.
(define (db-info-server server)
  (format #t "(db-info-server ~a)~%" server)
  "PB SERVER")

;; This callback gets called by the Control Protocol Server.  It
;; returns a single-line string about each client attached to a
;; server.
(define (db-info-client server socket)
  (format #t "(db-info-client ~a ~a)~%" server socket)
  "PB CLIENT")

;; This is an on-idle function called every second if nothing
;; else is happening.
(define (db-notify server)
  ;; (format #t "(db-notify ~a)~%" server)
  ;; What should I return?
  0)

;; This callback gets called when Serveez receives SIGHUP.
(define (db-reset server)
  (format #t "(db-reset ~a)~%" server)
  ;; What should I return?
  0)

(define-servertype! 
  `(
    ;; Server type prefix (mandatory)
    (prefix         . "db")

    ;; Server type description (mandatory)
    (description    . "Project Burro debug console")

    ;; Protocol Detection (mandatory for TCP)
    (detect-proto   . ,db-detect-proto)

    ;; Global server type initialization (optional)
    (global-init    . ,db-global-init)

    ;; Server instance initialization (optional)
    (init           . ,db-init)

    ;; Server instance finalization (optional)
    (finalize       . ,db-finalize)

    ;; Global server type finalization (optional)
    (global-finalize . ,db-global-finalize)

    ;; Socket connection (mandatory for TCP)
    (connect-socket  . ,db-connect-socket)

    ;; Server instance info (optional)
    (info-server     . ,db-info-server)

    ;; Client info (optional)
    (info-client     . ,db-info-client)

    ;; Server instance reset callback
    (reset           . ,db-reset)
    
    ;; Server instance notifier
    (notify          . ,db-notify)

    ;; Packet handler (mandatory for UDP)
    ;; (handle-request . ,db-handle-request)

    ;; Server type configuration
    (configuration   . ())))

;; === Client Handler Definition ==========================================

;; Called whenever new data has arrived.  The received buffer
;; can be peeked with `svz:sock:receive-buffer.  If there is
;; a complete request
;; - call `handle-request' telling it the length of the packet
;; - shrink the receive buffer
;; Return 0 on success or non-zero to shut down the connection.
(define (db-check-request socket)
  (format #t "(db-check-request ~a)~%" socket)
  (let ((ret 0))			; zero means good
    (while #t
	   (let* ((data (svz:sock:receive-buffer socket))
		  (index (bytevector-contains data (string->utf8 "\r\n")))
		  (message #f))
	     (if index
		 (begin
		   (set! message (subbytevector data 0 index))
		   (set! ret (db-handle-request socket message index))
		   (svz:sock:receive-buffer-reduce socket (+ index 2)))
		 ;; else
		 (break))))
    ret))

;; Once `check-request' has certified that there is a complete
;; packet of data, it passes it to this function for processing.
;; It is called by `db-check-request'.  It returns 0 for success.
;; Non-zero indicates that you want to shut down the connection.
(define (db-handle-request socket bytevector size)
  (format #t "(db-handle-request ~a ~a ~a)~%" socket bytevector size)
  (format #t "handling ~s~%" (utf8->string bytevector))
  (svz:sock:print socket (string->utf8 "NOOP\r\n"))
  0)

;; Called when a socket is lost for an external reason.
;; Set with `svz:sock:disconnected'.
(define (db-disconnected socket)
  ;; What should I return?
  0)

;; Called when serveez detects a flood or a buffer overflow.
;; Set with `svz:sock:kicked'
(define (db-kicked socket reason)
  ;; What should I return?
  0)

;; Called when the countdown timer drops to zero.
;; Set this callback with `svz:sock:idle'.  Set the 
;; callback timer with `svz:sock:idle-counter'
(define (db-idle socket)
  0)

;; Called once per server loop.  If it returns #t,
;; the trigger callback will be called.  Set
;; this with `svz:sock:trigger-condition'
(define (db-trigger-condition socket)
  #f)

;; Called each server loop that db-trigger-condition
;; returns #t.  Set this with `svz:sock:trigger'.
;; Returns 0 on success or non-zero to shut the
;; connection.
(define (db-trigger socket)
  0)

;; Port configuration
(define-port! 'db-port '((proto . tcp)
			 (port . 50002)))

;; Server instantiation
(define-server! 'db-server '())

(bind-server! 'db-port 'db-server)


