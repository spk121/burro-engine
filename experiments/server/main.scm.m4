;;; -*- mode: scheme -*-
;;; m4_changequote(`<<',`>>')
;;; m4_changecom(<<:M4_COMMENT_BEGIN:>>,<<:M4_COMMENT_END:>>)

(use-modules (serveez-mg core)
	     (serveez-mg lib)
	     (game)
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
