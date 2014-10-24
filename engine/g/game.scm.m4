;;; -*- mode: scheme -*-

;;; m4_changecom(`M4_COMMENT_BEGIN', `M4_COMMENT_END')
;;; m4_changequote(`<<',`>>')

;;; M4_COMMENT_BEGIN
;;; This m4-wrapped scheme file is used to assemble the complete
;;; Scheme game program as a single source file.  We do this because
;;; it would be too annoying to re-implement per-file loading in Guile
;;; using the glib-resources.
;;; M4_COMMENT_END

m4_sinclude(<<main.scm>>)
