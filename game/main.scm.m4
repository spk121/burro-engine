;;; -*- mode: scheme -*-
;;; m4_changequote(`<<',`>>')
;;; m4_changecom(<<:M4_COMMENT_BEGIN:>>,<<:M4_COMMENT_END:>>)

(use-modules (serveez-mg lib)
	     m4_ifelse(GUILE_VERSION,
		       <<1.8>>,
		       <<>>,
		       <<(rnrs bytevectors)>>))
	     
;; The Dynamic Ports should be between 49152 and 65535
(define +base-port+ 50000)

;; === Greet the user ========================================================
(printsln " "
	  "** Welcome to Serveez" serveez-version 
	  "using Guile" guile-version
	  (if have-debug
	      "(debugging enabled)"
	      ""
	   ))

(if have-Win32
    (println "* This is the Win32 port, good luck."))

(println "Serveez interfaces: " (serveez-interfaces))

(loadpath-add! "/usr/local/lib")
(println "Serveez loadpath: " (serveez-loadpath))

;; === Load game scripts =====================================================

(println "Loading game")
;;(include "/home/mike/Documents/burro-engine/game/game.scm")
(println "Game loaded")

(define-public (pb-init server)
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


(define (strip-boundary str)
  (string-trim-right str (string->char-set "\r\n")))

;; The request handler.
;; If it receives "HELLO", return "HELLO, WORLD!".
;; If it receives "WHAT TIME IT IS?", return the time.
;; Otherwise, return ERROR.
(define (pb-handle-request socket request len)
  (let ((command (strip-boundary (utf8->string request))))
    (format #t "Handling request '~s'~%" command)
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

;; The connect socket function: after a client has been detected by
;; detect-proto, this function handles each request
(define (pb-connect-socket server socket)
  ;; Each packet is going to be delimited by CR/LF
  (svz:sock:boundary socket "\r\n")
  ;; Connect to the request handler
  (svz:sock:handle-request socket pb-handle-request)
  ;; Zero indicates success
  0)


;; Control protocol server for remote control. ===============================
(define-port! 'control-port `(
			      (proto . tcp)
			      (port . ,+base-port+)
			      (ipaddr . *)
			      ))

(define-server! 'control-server)

(bind-server! 'control-port 'control-server)


;; === Game Document Web Server. =============================================
(define-port! 'http-port `(("proto"  . "tcp")
			   ("port"   . ,(+ 1 +base-port+))
			   ("ipaddr" . "*")))

(define-server! 'http-server '(
	      ;; standard properties
              ("admin"        . "project-burro@lonelycactus.com")
              ;; ("host"         . "lonelycactus.com")
              ("logfile"      . "http-access.log")
              ("logformat"    . "%h %i %u [%t] \"%R\" %c %l")
              ("indexfile"    . "index.html")
              ("docs"         . "/usr/local/share/serveez-mg/www")
              ("type-file"    . "/etc/mime.types")
              ("default-type" . "text/plain")
              ("nslookup"     . on)
              ("ident"        . yes)

              ;; cgi interface
              ("cgi-url"         . "/cgi-bin")
              ("cgi-dir"         . "../cgibin")
              ("cgi-application" . (("pl"  . "perl")
                                    ("scm" . "sizzle -s")))
	      ))

(bind-server! 'http-port 'http-server)

;; Game App server. =======================================================

;; Port configuration
(define-port! 'app-port `((proto . tcp)
			  (port . ,(+ 2 +base-port+))))

;; Servertype definitions
(define-servertype! 
  `((prefix  . "app")
    (description . "project-burro game engine")
    (detect-proto . ,pb-detect-proto)
    (init . ,pb-init)
    (connect-socket . ,pb-connect-socket)
    (configuration . ())
    ))

;; Server instantiation
(define-server! 'app-server '())

(bind-server! 'app-port 'app-server)

;; === general options for serveez ===========================================

;; log level 0..4 (lesser values mean fewer logging messages)
;; use 0 for production environment
(serveez-verbosity 4)

;; maximum accepted remote connections
(serveez-maxsockets 100)

;; password for the control protocol (plain/crypted)
;(serveez-passwd "password")
(serveez-passwd "heFw0NKyvGSTg")

;; === Return non-false for success ==========================================

#t
