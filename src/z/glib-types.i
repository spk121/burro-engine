%typemap(in) gint8, gint16, gint32, gint64, gint, gshort, glong {
        $1 = ($1_type)scm_to_long($input);
}

%typemap(out) gint8, gint16, gint32, gint64, gint, gshort, glong {
        $result = scm_from_long($1);
}

%typemap(in) guint8, guint16, guint32, guint64, guint, gushort, gulong {
        $1 = ($1_type)scm_to_ulong($input);
}

%typemap(out) guint8, guint16, guint32, guint64, guint, gushort, gulong {
        $result = scm_from_ulong($1);
}

%typemap(in) gfloat, gdouble {
        $1 = ($1_type)scm_to_double($input);
}

%typemap(out) gfloat, gdouble {
        $result = scm_from_double($1);
}

