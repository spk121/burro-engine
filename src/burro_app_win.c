#include <gtk/gtk.h>
#include <libguile.h>

#include "burro_app.h"
#include "burro_app_win.h"
#include "burro_canvas.h"
#include "burro_repl.h"
#include "burro_config_keys.h"
#include "burro_lisp.h"

static void console_write_icon (const gchar *c_icon_name);

struct _BurroAppWindow
{
    GtkApplicationWindow parent;
    GtkAccelGroup *accels;
    BurroCanvas *canvas;
    GtkMenuButton *gears;
    GtkRevealer *tools_revealer;
    GtkTextView *message_view;
    GtkTextBuffer *message_store;
    GtkScrolledWindow *message_scrolled_window;
    GtkEntry *console_entry;
    GtkComboBoxText *repl_combobox;
    GtkTreeView *vram_tree_view;
    GtkListStore *vram_list_store;
    
    // 60fps timer
    guint tick_timer;
    SCM tick_func;
    
    // Guile support
    BurroRepl *repl;
    SCM burro_module;           /* The (burro) module */
    SCM sandbox;                /* Opened files are parsed into this
                                 * anonymous module */
    SCM button_press_func;      /* Scheme callback for button press
                                 * events */
    // Log handler
    guint log_handler_id;
};

static void
action_open (GSimpleAction *simple,
             GVariant      *parameter,
             gpointer       user_data)
{
    GtkWindow *parent_window = GTK_WINDOW(user_data);
    GtkWidget *dialog;
    GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;
    gint res;

    GtkFileFilter *filter = gtk_file_filter_new();
    gtk_file_filter_set_name (filter, "Burro games");
    // gtk_file_filter_add_pattern(filter, "*.burro");
    gtk_file_filter_add_pattern(filter, "*.scm");

    dialog = gtk_file_chooser_dialog_new ("Load File",
                                          parent_window,
                                          action,
                                          "_Cancel",
                                          GTK_RESPONSE_CANCEL,
                                          "_Open",
                                          GTK_RESPONSE_ACCEPT,
                                          NULL);

   gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (dialog),
                                 filter);
                                 
    res = gtk_dialog_run (GTK_DIALOG (dialog));
    if (res == GTK_RESPONSE_ACCEPT)
    {
        GtkFileChooser *chooser = GTK_FILE_CHOOSER (dialog);
        GFile *file = gtk_file_chooser_get_file (chooser);
        gtk_widget_destroy (dialog);
        g_object_unref (filter);
        burro_app_window_open (BURRO_APP_WINDOW (parent_window), file);
        g_object_unref (file);
    }
    else {
        g_object_unref (filter);
        gtk_widget_destroy (dialog);
    }
}

static void
action_view_tools (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
{
	BurroAppWindow *window;
	gboolean visible;

	window = BURRO_APP_WINDOW (user_data);

    // gtk_revealer_set_transition_duration(window->tools_revealer, 2000);
	visible = gtk_revealer_get_reveal_child (window->tools_revealer);
    if (visible)
    {
        int width, height;
        gtk_window_get_size (GTK_WINDOW(window), &width, &height);

        gtk_revealer_set_reveal_child (window->tools_revealer, FALSE);
        gtk_widget_set_visible (GTK_WIDGET(window->tools_revealer), FALSE);
        gtk_window_resize (GTK_WINDOW(window), width, 384);
    }
    else
    {
        gtk_widget_set_visible (GTK_WIDGET(window->tools_revealer), TRUE);
        gtk_revealer_set_reveal_child (window->tools_revealer, TRUE);
    }
}

static void
action_run_repl (GSimpleAction *simple,
                 GVariant      *state,
                 gpointer       user_data)
{
    g_critical ("action_run_repl is broken");
	BurroAppWindow *window;
	gboolean running;

	window = BURRO_APP_WINDOW (user_data);

	running = g_variant_get_boolean (state);
}


static GActionEntry win_entries[] =
{
    /* Stateless actions. */
    {"open",  action_open,  NULL, NULL, NULL},
    {"view-tools", action_view_tools, NULL, NULL, NULL},
};

static gboolean
accel_action_view_tools (GtkAccelGroup *accel_group,
                         GObject *acceleratable,
                         guint keyval,
                         GdkModifierType modifier)
{
	BurroAppWindow *window;
	window = BURRO_APP_WINDOW (acceleratable);
    gboolean visible = gtk_revealer_get_reveal_child (window->tools_revealer);
    if (visible)
    {
        int width, height;
        gtk_window_get_size (GTK_WINDOW(window), &width, &height);

        gtk_revealer_set_reveal_child (window->tools_revealer, FALSE);
        gtk_widget_set_visible (GTK_WIDGET(window->tools_revealer), FALSE);
        gtk_window_resize (GTK_WINDOW(window), width, 384);
    }
    else
    {
        gtk_widget_set_visible (GTK_WIDGET(window->tools_revealer), TRUE);
        gtk_revealer_set_reveal_child (window->tools_revealer, TRUE);
    }
}

static void
signal_action_repl_enabled (GtkComboBox *widget,
                            gpointer user_data)
{
    gint enabled = gtk_combo_box_get_active (widget);
    if (enabled)
    {
        burro_repl_enable (BURRO_REPL(user_data));
        gtk_widget_set_sensitive (GTK_WIDGET(widget), FALSE);
    }
}


G_DEFINE_TYPE(BurroAppWindow, burro_app_window, GTK_TYPE_APPLICATION_WINDOW);

static BurroAppWindow *app_window_cur;

static void
signal_action_console_activate (GtkEntry *entry, gpointer user_data)
{
    const char* txt = gtk_entry_get_text (entry);
    SCM func = scm_c_public_ref("burro", "eval-string-in-sandbox");
    SCM ret = scm_call_2(func,
                         scm_from_utf8_string(txt),
                         app_window_cur->sandbox);
    char *output = scm_to_utf8_string (ret);
    
    GtkTextIter iter;
    GtkTextBuffer *msgstore = app_window_cur->message_store;
    gtk_text_buffer_get_end_iter (msgstore, &iter);
    gtk_text_buffer_place_cursor (msgstore, &iter);
    gtk_text_buffer_insert_at_cursor (msgstore, ">>\t", -1);
    gtk_text_buffer_insert_at_cursor (msgstore, txt, -1);
    gtk_text_buffer_insert_at_cursor (msgstore, "\n", -1);
    if (g_strcmp0 (output, "#<unspecified>") != 0)
    {
        if (strncmp (output, "ERROR:", 6) == 0)
        {
            console_write_icon ("dialog-error");
            gtk_text_buffer_insert_at_cursor (msgstore, output + strlen("ERROR:"), -1);
        }
        else
        {
            gtk_text_buffer_insert_at_cursor (msgstore, "<<\t", -1);
            gtk_text_buffer_insert_at_cursor (msgstore,
                                              output, -1);
        }
        gtk_text_buffer_insert_at_cursor (msgstore, "\n", -1);
    }
    gtk_text_buffer_get_end_iter (msgstore, &iter);
    gtk_text_buffer_place_cursor (msgstore, &iter);
    
#if 0
    gtk_text_view_scroll_to_iter (app_window_cur->message_view,
                                  &iter,
                                  0.0, TRUE, 0.0, 1.0);
#endif
    GtkAdjustment *vadj
        = gtk_scrolled_window_get_vadjustment(app_window_cur->message_scrolled_window);
    gtk_adjustment_set_value(vadj,
                             gtk_adjustment_get_upper(vadj)
                             - gtk_adjustment_get_page_size(vadj));
}

static gboolean
signal_action_button_press_event (GtkWidget *widget, GdkEventButton *event,
                                  gpointer user_data)
{
    if (scm_is_true (scm_procedure_p (app_window_cur->button_press_func)))
    {
        SCM func = scm_c_public_ref("burro", "call-with-limits");
        SCM ret = scm_call_4(func,
                             app_window_cur->button_press_func,
                             scm_from_double (1.0e-6 * event->time),
                             scm_from_double (event->x),
                             scm_from_double (event->y));
        if (scm_is_string (ret))
            scm_call_1(scm_c_public_ref("burro", "console-error"), ret);
    }
    return TRUE;
}

static gboolean
timeout_action_tick (gpointer user_data)
{
    if (scm_is_true (scm_procedure_p (app_window_cur->tick_func)))
    {
        SCM func = scm_c_public_ref("burro", "call-with-limits");
        SCM ret = scm_call_2(func,
                             app_window_cur->tick_func,
                             scm_from_double (1.0e-6 * g_get_monotonic_time()));
        if (scm_is_string (ret))
            scm_call_1(scm_c_public_ref("burro", "console-error"), ret);
    }
    return TRUE;
}

static void
timeout_action_destroy (gpointer data)
{
    
}

static void
log_func (const gchar *log_domain,
          GLogLevelFlags log_level,
          const gchar *message,
          gpointer user_data)
{
    GtkTextBuffer *msgstore = app_window_cur->message_store;
    GtkTextIter iter;
    gtk_text_buffer_get_end_iter (msgstore, &iter);
    gtk_text_buffer_place_cursor (msgstore, &iter);

    if (log_level == G_LOG_LEVEL_ERROR
        || log_level == G_LOG_LEVEL_CRITICAL)
        console_write_icon ("dialog-error");
    else if (log_level == G_LOG_LEVEL_WARNING)
        console_write_icon ("dialog-warning");
    else
        console_write_icon ("dialog-information");

    gtk_text_buffer_insert_at_cursor (msgstore, "\t", -1); 
    gtk_text_buffer_insert_at_cursor (msgstore, message, -1); 
    gtk_text_buffer_insert_at_cursor (msgstore, "\n", -1); 
    gtk_text_buffer_get_end_iter (msgstore, &iter);

    gtk_text_view_scroll_to_iter (app_window_cur->message_view,
                                  &iter,
                                  0.0, TRUE, 0.0, 1.0);
    GtkAdjustment *vadj
        = gtk_scrolled_window_get_vadjustment(app_window_cur->message_scrolled_window);
    gtk_adjustment_set_value(vadj,
                             gtk_adjustment_get_upper(vadj)
                             - gtk_adjustment_get_page_size(vadj));
}

static void
burro_app_window_init (BurroAppWindow *win)
{
    GtkBuilder *builder;
    GMenuModel *menu;
    GAction *action;

    gtk_widget_init_template (GTK_WIDGET (win));

    // Construct the menu
    g_action_map_add_action_entries (G_ACTION_MAP (win),
                                     win_entries, G_N_ELEMENTS (win_entries),
                                     win);
                                
    builder = gtk_builder_new_from_resource ("/com/lonelycactus/burroengine/gears-menu.ui");
                                
    menu = G_MENU_MODEL (gtk_builder_get_object (builder, "menu"));
    gtk_menu_button_set_menu_model (GTK_MENU_BUTTON (win->gears), menu);
    g_object_unref (builder);

    GtkAccelGroup *accel = gtk_accel_group_new();
    GClosure *cl_view = g_cclosure_new(G_CALLBACK(accel_action_view_tools),
                                       win, NULL);
    gtk_accel_group_connect (accel,
                             GDK_KEY_F12,
                             0,
                             GTK_ACCEL_VISIBLE,
                             cl_view);
    gtk_window_add_accel_group(GTK_WINDOW (win),
                               accel);
    
    
    gtk_application_window_set_show_menubar (GTK_APPLICATION_WINDOW (win), TRUE);
    
    // Attach a text storage to the text box
    win->message_store = gtk_text_view_get_buffer (win->message_view);
    
    // Set the visibility of the text box
    GAction *vdebug =
        g_action_map_lookup_action (G_ACTION_MAP (win), "view-debug");

    win->burro_module = burro_lisp_new ();
    win->sandbox = SCM_BOOL_F;

    // FIXME: how to do this properly
    // Really shrink the window down to minimal
    gtk_window_resize (GTK_WINDOW(win), 512, 384);
    
    // Set up console entry
    g_signal_connect (win->console_entry,
                      "activate",
                      G_CALLBACK(signal_action_console_activate),
                      NULL);
    
    // Set up REPL
    gtk_combo_box_set_active (GTK_COMBO_BOX (win->repl_combobox), 0);
    win->repl = g_object_new (BURRO_TYPE_REPL, NULL);
    g_signal_connect (win->repl_combobox,
                      "changed",
                      G_CALLBACK(signal_action_repl_enabled),
                      win->repl);

    // Hook up with canvas
    win->button_press_func = SCM_BOOL_F;
    g_signal_connect (G_OBJECT (win->canvas),
                      "button-press-event",
                      G_CALLBACK (signal_action_button_press_event),
                      NULL);

    // Hook up the VRAM viewer
    win->vram_list_store = vram_info_list_store_new ();
    gtk_tree_view_set_model (win->vram_tree_view, GTK_TREE_MODEL (win->vram_list_store));
    {
        GtkCellRenderer *renderer;
        GtkTreeViewColumn *column;

        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes ("Bank",
                                                           renderer,
                                                           "text", VRAM_COLUMN_NAME,
                                                           NULL);
        gtk_tree_view_append_column (GTK_TREE_VIEW (win->vram_tree_view), column); 
        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes ("Type",
                                                           renderer,
                                                           "text", VRAM_COLUMN_TYPE,
                                                           NULL);
        gtk_tree_view_append_column (GTK_TREE_VIEW (win->vram_tree_view), column); 
        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes ("Filename",
                                                           renderer,
                                                           "text", VRAM_COLUMN_FILENAME,
                                                           NULL);
        gtk_tree_view_append_column (GTK_TREE_VIEW (win->vram_tree_view), column); 
        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes ("Size",
                                                           renderer,
                                                           "text", VRAM_COLUMN_SIZE,
                                                           NULL);
        gtk_tree_view_append_column (GTK_TREE_VIEW (win->vram_tree_view), column); 
    }
    gtk_widget_show (GTK_WIDGET(win->vram_tree_view));
    
    
    // Animation timer: about 60fps
    win->tick_func = SCM_BOOL_F;
    win->tick_timer = g_timeout_add_full (G_PRIORITY_DEFAULT,
                                          12, /* milliseconds */
                                          timeout_action_tick,
                                          (gpointer) win,
                                          timeout_action_destroy);

    // Alternative logging
    win->log_handler_id = g_log_set_handler_full (NULL,
                                                  G_LOG_LEVEL_ERROR
                                                  | G_LOG_LEVEL_CRITICAL
                                                  | G_LOG_LEVEL_WARNING
                                                  | G_LOG_LEVEL_MESSAGE
                                                  | G_LOG_LEVEL_INFO,
                                                  log_func,
                                                  NULL,
                                                  NULL);
                                                  
                                                  
}

static void
burro_app_window_dispose (GObject *object)
{
    BurroAppWindow *win;

    win = BURRO_APP_WINDOW (object);

    g_clear_object (&win->repl);
    G_OBJECT_CLASS (burro_app_window_parent_class)->dispose (object);
}

static void
burro_app_window_class_init (BurroAppWindowClass *class)
{
    G_OBJECT_CLASS (class)->dispose = burro_app_window_dispose;

    gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (class),
                                                 "/com/lonelycactus/burroengine/window.ui");

    // gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, fixed);
    gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, canvas);
    gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, gears);
    gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, tools_revealer);
    gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, message_view);
    gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, message_scrolled_window);
    gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, repl_combobox);
    gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, console_entry);
    gtk_widget_class_bind_template_child (GTK_WIDGET_CLASS (class), BurroAppWindow, vram_tree_view);
}

BurroAppWindow *
burro_app_window_new (BurroApp *app)
{
    app_window_cur = g_object_new (BURRO_APP_WINDOW_TYPE, "application", app, NULL);
    char *err_string;
    app_window_cur->sandbox = burro_make_sandbox (NULL, &err_string);
    return app_window_cur;
}    

void
burro_app_window_open (BurroAppWindow *win,
                       GFile *file)
{
    // FIXME: when we open with the OPEN signal, load the file into SANDBOX
    char *err_string;
    win->sandbox = burro_make_sandbox (file, &err_string);

    if (scm_is_false (win->sandbox))
    {
        char *filename = g_file_get_basename (file);
        
        GtkDialogFlags flags = GTK_DIALOG_DESTROY_WITH_PARENT;
        GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                         flags,
                                         GTK_MESSAGE_ERROR,
                                         GTK_BUTTONS_CLOSE,
                                         "There is a problem with “%s”.\n%s",
                                         filename,
                                         err_string);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        g_free (filename);
    }
    g_object_unref (file);
}

SCM_DEFINE (G_burro_app_win_get_sandbox, "get-sandbox", 0, 0, 0,
            (void), "\
Return the current sandbox.")
{
    return app_window_cur->sandbox;
}

SCM_DEFINE (G_burro_app_win_set_title, "set-title", 1, 0, 0,
            (SCM title), "\
Set the title in titlebar of the main window.")
{
    if (scm_is_string (title))
    {
        char *str = scm_to_utf8_string(title);
        gtk_window_set_title (GTK_WINDOW(app_window_cur), str);
        free (str);
    }
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_burro_app_win_receive_button_presses,
            "receive-button-presses", 1, 0, 0,
            (SCM proc), "\
Registers PROC -- a procedure that takes 3 values (time, x, y) -- as\n\
the procedure to receive button press events.  To ignore button\n\
presses, PROC can be #f.  Returns the previously registered procedure,\n\
if any.")
{
    SCM prev = app_window_cur->button_press_func;
    app_window_cur->button_press_func = proc;
    return prev;
}

SCM_DEFINE (G_burro_app_win_receive_clock_tick,
            "receive-clock-tick", 1, 0, 0,
            (SCM proc), "\
Registers PROC -- a procedure that takes 1 value: time in seconds --\n\
as the procedure to receive timer events about 60 times per second.\n\
To ignore tick events, PROC can be #f.  Returns the previously\n\
registered procedure, if any.")
{
    SCM prev = app_window_cur->tick_func;
    app_window_cur->tick_func = proc;
    return prev;
}

SCM_DEFINE (G_burro_app_win_console_write, "console-write-bytevector", 3, 0, 0,
            (SCM bv, SCM _index, SCM _count), "\
Takes the bytes from STORE between INDEX and INDEX + COUNT and writes\n\
them to Message window.  It returns the number of bytes actually\n\
written.")
{
    GtkTextBuffer *msgstore = app_window_cur->message_store;
    size_t count = scm_to_size_t (_count);
    size_t index = scm_to_size_t (_index);
    if (count == 0)
        return 0;

    // The sandbox should never be allowed to fill up all the memory
    // with junk text.
    gint totsize = gtk_text_buffer_get_char_count (msgstore);
    if (totsize > 10*1024)
    {
        GtkTextIter start, halfway;
        gtk_text_buffer_get_iter_at_offset (msgstore, &start, 0);
        gtk_text_buffer_get_iter_at_offset (msgstore, &halfway, totsize / 2);
        gtk_text_buffer_delete (msgstore, &start, &halfway);
    }
    
    GtkTextIter iter;
    gtk_text_buffer_get_end_iter (msgstore, &iter);
    gtk_text_buffer_place_cursor (msgstore, &iter);
    gtk_text_buffer_insert_at_cursor (msgstore,
                                      SCM_BYTEVECTOR_CONTENTS(bv) + index,
                                      count);
    gtk_text_buffer_get_end_iter (msgstore, &iter);

    gtk_text_view_scroll_to_iter (app_window_cur->message_view,
                                  &iter,
                                  0.0, TRUE, 0.0, 1.0);
    GtkAdjustment *vadj
        = gtk_scrolled_window_get_vadjustment(app_window_cur->message_scrolled_window);
    gtk_adjustment_set_value(vadj,
                             gtk_adjustment_get_upper(vadj)
                             - gtk_adjustment_get_page_size(vadj));
    
    return _count;
}

static void
console_write_icon (const gchar *c_icon_name)
{
    GError *error = NULL;
    GtkIconTheme *icon_theme;
    GdkPixbuf *pixbuf;

    icon_theme = gtk_icon_theme_get_default ();
    pixbuf = gtk_icon_theme_load_icon (icon_theme,
                                       c_icon_name, // icon name
                                       12, // icon size
                                       0,  // flags
                                       &error);
    if (!pixbuf)
    {
        g_warning ("Couldn’t load icon: %s", error->message);
        g_error_free (error);
    }

    // Use the pixbuf
    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_mark (app_window_cur->message_store,
                                      &iter,
                                      gtk_text_buffer_get_insert (app_window_cur->message_store));
    
    gtk_text_buffer_insert_pixbuf(app_window_cur->message_store,
                                  &iter,
                                  pixbuf);
    g_object_unref (pixbuf);
}

SCM_DEFINE (G_burro_app_win_console_write_icon,
            "console-write-icon", 1, 0, 0,
            (SCM name), "\
Writes the icon, given by its icon theme name, to the console.")
{
    char *c_icon_name = scm_to_utf8_string (name);
    console_write_icon (c_icon_name);
    free (c_icon_name);
    return SCM_BOOL_T;
}

void
burro_app_win_init_guile_procedures ()
{
#include "burro_app_win.x"
    scm_c_export ("get-sandbox",
                  "console-write-bytevector",
                  "console-write-icon",
                  "receive-button-presses",
                  "receive-tick",
                  "receive-clock-tick",
                  "set-title",
                  NULL);
}


/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/

