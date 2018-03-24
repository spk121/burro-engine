#include <gtk/gtk.h>
#include <libguile.h>

#include "burro_app.h"
#include "burro_app_win.h"
#include "burro_canvas.h"
#include "burro_repl.h"
#include "burro_config_keys.h"
#include "burro_lisp.h"

struct _BurroAppWindow
{
    GtkApplicationWindow parent;
    GtkAccelGroup *accels;
    BurroCanvas *canvas;
    GtkMenuButton *gears;
    GtkRevealer *tools_revealer;
    GtkTextView *message_view;
    GtkTextBuffer *message_store;
    BurroRepl *repl;

    SCM burroscript;
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

	visible = gtk_revealer_get_reveal_child (window->tools_revealer);
    gtk_revealer_set_reveal_child (window->tools_revealer, !visible);
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
    gboolean revealed = gtk_revealer_get_reveal_child (window->tools_revealer);
    gtk_revealer_set_reveal_child (window->tools_revealer, !revealed);
}

G_DEFINE_TYPE(BurroAppWindow, burro_app_window, GTK_TYPE_APPLICATION_WINDOW);

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
    gtk_text_buffer_set_text (win->message_store, "Beginning initialization\n", -1);
    
    // Set the visibility of the text box
    GAction *vdebug =
        g_action_map_lookup_action (G_ACTION_MAP (win), "view-debug");
    gboolean visible =
        g_variant_get_boolean (g_action_get_state (vdebug));

    if (visible)
        gtk_revealer_set_reveal_child (win->tools_revealer, TRUE);
    else
        gtk_revealer_set_reveal_child (win->tools_revealer, FALSE);

    char *err_string = NULL;
    win->burroscript = burro_lisp_new (&err_string);
    if (scm_is_false (win->burroscript)) {
        g_critical ("The script engine failed to load: %s", err_string);
        g_free (err_string);
    }
        
    // win->repl = g_object_new (BURRO_TYPE_REPL, NULL);
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
}

BurroAppWindow *
burro_app_window_new (BurroApp *app)
{
    return g_object_new (BURRO_APP_WINDOW_TYPE, "application", app, NULL);
}

void
burro_app_window_open (BurroAppWindow *win,
                       GFile *file)
{
    char *err_string;
    gboolean ok = burro_lisp_load (win->burroscript, file, &err_string);

    if (!ok)
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


/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/

