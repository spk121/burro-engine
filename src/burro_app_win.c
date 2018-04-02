#include <gtk/gtk.h>
#include <libguile.h>

#include "burro_app.h"
#include "burro_app_win.h"
#include "burro_canvas.h"
#include "burro_repl.h"
#include "burro_config_keys.h"
#include "burro_lisp.h"

#define GAME_LOOP_MINIMUM_PERIOD_MICROSECONDS (1000000 / 60)
#define GAME_LOOP_IDEAL_PERIOD_MICROSECONDS (1000000 / 30)
#define GAME_LOOP_MAXIMUM_PERIOD_MICROSECONDS (1000000 / 10)

enum {
    DEBUG_COLUMN_TIME,                /* The name of the bank, like "VRAM A" */
    DEBUG_COLUMN_NAME,
    DEBUG_COLUMN_VALUE,
    DEBUG_COLUMN_STACK,
    DEBUG_N_COLUMNS
};


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
    GtkLabel *update_rate_label;
    GtkLabel *duty_cycle_label;
    GtkToggleButton *debug_pause_button;
    GtkButton *debug_next_button;
    GtkButton *debug_refresh_button;
    GtkListStore *debug_peek_list_store;
    GtkTreeView *debug_peek_tree_view;
    
    // The main game loop is in the idle handler.
    gboolean minimized_flag;
    gboolean maximized_flag;
    guint game_loop_callback_id;
    gint game_loop_frame_count;
    gboolean game_loop_active_flag;
    gboolean game_loop_step_flag;
    gboolean game_loop_quitting;
    gint64 game_loop_start_time;
    gint64 game_loop_end_time;
    gint64 game_loop_start_time_prev;
    gboolean game_loop_full_speed;
    gint64 game_loop_avg_period;
    gint64 game_loop_avg_duration;
    
    gboolean have_mouse_move_event;
    double mouse_move_x;
    double mouse_move_y;
    gboolean have_mouse_click_event;
    double mouse_click_x;
    double mouse_click_y;
    gboolean have_text_move_event;
    int text_move_location;
    gboolean have_text_click_event;
    int text_click_location;
    
    // Guile support
    BurroRepl *repl;
    SCM burro_module;           /* The (burro) module */
    SCM sandbox;                /* Opened files are parsed into this
                                 * anonymous module */
    // Log handler
    guint log_handler_id;
};


GtkListStore *debug_peek_list_store_new()
{
    GtkListStore *list_store;

    list_store = gtk_list_store_new (DEBUG_N_COLUMNS,
                                     G_TYPE_STRING,
                                     G_TYPE_STRING,
                                     G_TYPE_STRING,
                                     G_TYPE_STRING);
    return list_store;
}

#if 0
void
debug_peek_list_store_update(GtkListStore *list_store)
{
    GtkTreePath *path;
    GtkTreeIter iter;
    gint i;
    const char *nullstr = "";

    for (i = VRAM_A; i < VRAM_COUNT; i ++)
    {
        path = gtk_tree_path_new_from_indices (i - VRAM_A, -1);
        gtk_tree_model_get_iter(GTK_TREE_MODEL (list_store), &iter, path);
        
        char *siz = vram_size_string(i);
        // gtk_list_store_append (list_store, &iter);
        gtk_list_store_set (list_store, &iter,
                            VRAM_COLUMN_NAME, vram_bank_name[i],
                            VRAM_COLUMN_TYPE, vram_type_name[vram_info[i].type],
                            VRAM_COLUMN_FILENAME, (vram_info[i].filename
                                              ? vram_info[i].filename
                                              : nullstr),
                            VRAM_COLUMN_SIZE, siz,
                            -1);
        g_free (siz);
    }
}
#endif

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
    return TRUE;
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
signal_action_canvas_button_press_event (GtkWidget *widget, GdkEventButton *event,
                                  gpointer user_data)
{
    GdkEventButton *button = event;
    if (button->type == GDK_BUTTON_PRESS)
    {
        app_window_cur->have_mouse_click_event = TRUE;
        app_window_cur->mouse_click_x = button->x;
        app_window_cur->mouse_click_y = button->y;
        int index, trailing;
        if (burro_canvas_xy_to_index (app_window_cur->canvas, button->x, button->y, &index, &trailing))
        {
            app_window_cur->have_text_click_event = TRUE;
            app_window_cur->text_click_location = index;
        }
    }
    
    return TRUE;
}

static gboolean
signal_action_canvas_motion_notify_event (GtkWidget *widget, GdkEventMotion *event,
                                          gpointer user_data)
{
    app_window_cur->have_mouse_move_event = TRUE;
    app_window_cur->mouse_move_x = event->x;
    app_window_cur->mouse_move_y = event->y;
    int index, trailing;
    if (burro_canvas_xy_to_index (app_window_cur->canvas, event->x, event->y, &index, &trailing))
    {
        app_window_cur->have_text_move_event = TRUE;
        app_window_cur->text_move_location = index;
    }
    else
    {
        app_window_cur->have_text_move_event = TRUE;
        app_window_cur->text_move_location = -1;
    }
        
    return TRUE;
}

static void
signal_debug_pause (GtkButton *button, gpointer user_data)
{
    app_window_cur->game_loop_active_flag = !app_window_cur->game_loop_active_flag;
    if (app_window_cur->game_loop_active_flag)
        gtk_widget_set_sensitive (GTK_WIDGET(app_window_cur->debug_next_button), FALSE);
    else
    {
        gtk_widget_set_sensitive (GTK_WIDGET(app_window_cur->debug_next_button), TRUE);
        gtk_label_set_text (app_window_cur->update_rate_label, "paused");
    }
}

static void
signal_debug_next (GtkButton *button, gpointer user_data)
{
    app_window_cur->game_loop_step_flag = TRUE;
}

static void
signal_debug_refresh (GtkButton *button, gpointer user_data)
{
    
}

static gboolean
game_loop (gpointer user_data)
{
    BurroAppWindow *win = BURRO_APP_WINDOW (user_data);
    
    if (win->game_loop_quitting)
        return FALSE;

    if (!win->minimized_flag)
    {
        if (win->game_loop_active_flag || win->game_loop_step_flag)
        {
            win->game_loop_step_flag = FALSE;
            
            gint64 start_time = g_get_monotonic_time();
            if (win->game_loop_full_speed || ((start_time - win->game_loop_start_time) > GAME_LOOP_MINIMUM_PERIOD_MICROSECONDS))
            {
                win->game_loop_start_time_prev = win->game_loop_start_time;
                win->game_loop_start_time = start_time;
                gint64 dt = start_time - win->game_loop_start_time_prev;
                
                // If dt is too large, maybe we're resuming from a break point.
                // Frame-lock to the target rate for this frame.
                if (dt > GAME_LOOP_MAXIMUM_PERIOD_MICROSECONDS)
                    dt = GAME_LOOP_IDEAL_PERIOD_MICROSECONDS;
                
                // Update the TCP repl, if it is running
                //repl_tick();
                
                // Pass any new mouse clicks and other events to the process manager
                if (win->have_mouse_move_event)
                {
                    // g_message("sending mouse move to pm, %f %f", win->mouse_move_x, win->mouse_move_y);
                    scm_call_2 (scm_c_public_ref ("burro pm", "pm-set-mouse-move"),
                                scm_from_double (win->mouse_move_x),
                                scm_from_double (win->mouse_move_y));
                    win->have_mouse_move_event = FALSE;
                }
                if (win->have_mouse_click_event)
                {
                    // g_message("sending mouse click to pm, %f %f", win->mouse_click_x, win->mouse_click_y);
                    scm_call_2 (scm_c_public_ref ("burro pm", "pm-set-mouse-click"),
                                scm_from_double (win->mouse_click_x),
                                scm_from_double (win->mouse_click_y));
                    win->have_mouse_click_event = FALSE;
                }
                if (win->have_text_move_event)
                {
                    // g_message("sending text move to pm, %d", win->text_move_location);
                    scm_call_1 (scm_c_public_ref ("burro pm", "pm-set-text-move"),
                                scm_from_int (win->text_move_location));
                    win->have_text_move_event = FALSE;
                }
                if (win->have_text_click_event)
                {
                    // g_message("sending text click to pm, %d", win->text_click_location);
                    scm_call_1 (scm_c_public_ref ("burro pm", "pm-set-text-click"),
                                scm_from_int (win->text_click_location));
                    win->have_text_click_event = FALSE;
                }

                // Let the guile processes run
                SCM ret = scm_call_1 (scm_c_public_ref ("burro pm", "pm-update-or-error-string"),
                                      scm_from_int64(dt));
                if (scm_is_string (ret))
                {
                    scm_call_1(scm_c_public_ref("burro debug", "console-error"),
                               ret);
                }
                // Update subsystems

                // Render things

                // Keep some statistics on frame rate, and computation time
                win->game_loop_frame_count ++;
                win->game_loop_avg_period = ((19 * win->game_loop_avg_period) / 20
                                                 + dt / 20);

                win->game_loop_end_time = g_get_monotonic_time();
                win->game_loop_avg_duration = ((19 * win->game_loop_avg_duration) / 20
                                               + (win->game_loop_end_time - win->game_loop_start_time) / 20);
                
                if (!(win->game_loop_frame_count % 60))
                {
                    char *update_rate_str = g_strdup_printf("%.1f", 1000000.0 / win->game_loop_avg_period);
                    char *duty_cycle_str = g_strdup_printf("%.1f%%", (100.0 * win->game_loop_avg_duration) / win->game_loop_avg_period);
                    gtk_label_set_text (win->update_rate_label, update_rate_str);
                    gtk_label_set_text (win->duty_cycle_label, duty_cycle_str);
                    g_free (duty_cycle_str);
                    g_free (update_rate_str);
                }
            }
            else /* We are running too fast. */
                usleep(1000);
        }
        else /* ! active */
        {
            usleep (1000);
            // audio pause
            // sleep until next event
        }
    }
    else /* minimized */
        usleep (1000);
         
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

    // The game loop needs to know about mouse moves and mouse clicks
    // that specifically happen in the canvas.
    // This is for mouse clicks.
    win->have_mouse_click_event = FALSE;
    win->have_text_click_event = FALSE;
    g_signal_connect (G_OBJECT (win->canvas),
                      "button-press-event",
                      G_CALLBACK (signal_action_canvas_button_press_event),
                      NULL);
    // This is for mouse moves
    win->have_mouse_move_event = FALSE;
    win->have_text_move_event = FALSE;
    g_signal_connect (G_OBJECT (win->canvas),
                      "motion-notify-event",
                      G_CALLBACK (signal_action_canvas_motion_notify_event),
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
    
    
    // Main Game Loop
    win->game_loop_active_flag = TRUE;
    win->game_loop_quitting = FALSE;
    win->game_loop_start_time = g_get_monotonic_time ();
    win->game_loop_end_time = g_get_monotonic_time ();
    win->game_loop_start_time_prev = win->game_loop_start_time - GAME_LOOP_IDEAL_PERIOD_MICROSECONDS;
    win->game_loop_full_speed = FALSE;
    win->game_loop_frame_count = 0;
    win->game_loop_avg_period = GAME_LOOP_IDEAL_PERIOD_MICROSECONDS;
    win->game_loop_avg_duration = GAME_LOOP_IDEAL_PERIOD_MICROSECONDS / 4;

    win->game_loop_callback_id = g_idle_add_full (G_PRIORITY_DEFAULT_IDLE,
                                       game_loop,
                                       (gpointer) win,
                                       timeout_action_destroy);

    // Debugger
    g_signal_connect (G_OBJECT (win->debug_pause_button),
                      "toggled",
                      G_CALLBACK (signal_debug_pause),
                      NULL);
    gtk_widget_set_sensitive (GTK_WIDGET (win->debug_pause_button), TRUE);
    g_signal_connect (G_OBJECT (win->debug_next_button),
                      "clicked",
                      G_CALLBACK (signal_debug_next),
                      NULL);
    g_signal_connect (G_OBJECT (win->debug_refresh_button),
                      "clicked",
                      G_CALLBACK (signal_debug_refresh),
                      NULL);
    // Hook up the peek viewer
    win->debug_peek_list_store = debug_peek_list_store_new ();
    gtk_tree_view_set_model (win->debug_peek_tree_view, GTK_TREE_MODEL (win->debug_peek_list_store));
    {
        GtkCellRenderer *renderer;
        GtkTreeViewColumn *column;

        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes ("Time",
                                                           renderer,
                                                           "text", DEBUG_COLUMN_TIME,
                                                           NULL);
        gtk_tree_view_append_column (GTK_TREE_VIEW (win->debug_peek_tree_view), column);
        
        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes ("Label",
                                                           renderer,
                                                           "text", DEBUG_COLUMN_NAME,
                                                           NULL);
        gtk_tree_view_append_column (GTK_TREE_VIEW (win->debug_peek_tree_view), column);
        
        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes ("Value",
                                                           renderer,
                                                           "text", DEBUG_COLUMN_VALUE,
                                                           NULL);
        gtk_tree_view_append_column (GTK_TREE_VIEW (win->debug_peek_tree_view), column);
        
        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes ("Stack",
                                                           renderer,
                                                           "text", DEBUG_COLUMN_STACK,
                                                           NULL);
        gtk_tree_view_append_column (GTK_TREE_VIEW (win->debug_peek_tree_view), column);
    }
    gtk_widget_show (GTK_WIDGET(win->debug_peek_tree_view));

    
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

// This callback is called when the window is maximizing and
// minimizing.
gboolean
burro_app_window_state (GtkWidget *widget,
                        GdkEventWindowState *win_event,
                        gpointer user_data)
{
    BurroAppWindow *win = BURRO_APP_WINDOW(widget);
    
    if (win_event->new_window_state & GDK_WINDOW_STATE_ICONIFIED)
    {
        win->minimized_flag = TRUE;
        win->maximized_flag = FALSE;
    }
    else if (win_event->new_window_state & (GDK_WINDOW_STATE_MAXIMIZED
                                            | GDK_WINDOW_STATE_FULLSCREEN))
    {
        win->minimized_flag = FALSE;
        win->maximized_flag = TRUE;
    }
    else
    {
        win->minimized_flag = FALSE;
        win->maximized_flag = FALSE;
    }
    return TRUE;
}

// This callback is called when the application has asked this window
// to delete itself.
static  gboolean
burro_app_window_delete (GtkWidget	     *widget,
                         GdkEventAny	     *event)
{
    gtk_widget_destroy(widget);
    return TRUE;
}

static void
burro_app_window_dispose (GObject *object)
{
    BurroAppWindow *win;
    win = BURRO_APP_WINDOW (object);

    g_log_remove_handler (NULL, win->log_handler_id);
    g_log_set_default_handler (g_log_default_handler, NULL);
    
    win->game_loop_quitting = TRUE;

    g_clear_object(&(win->vram_tree_view));
    if (win->canvas)
    {
        g_object_unref(win->canvas);
        win->canvas = NULL;
    }

    win->sandbox = SCM_BOOL_F;
    win->burro_module = SCM_BOOL_F;
    
    g_clear_object (&win->repl);
    G_OBJECT_CLASS (burro_app_window_parent_class)->dispose (object);
}

static void
burro_app_window_class_init (BurroAppWindowClass *class)
{
    GObjectClass *gclass = G_OBJECT_CLASS(class);
    GtkWidgetClass *gtkclass = GTK_WIDGET_CLASS (class);
    const char window_ui[] = "/com/lonelycactus/burroengine/window.ui";
    
    gclass->dispose = burro_app_window_dispose;

    gtkclass->delete_event = burro_app_window_delete;
    gtkclass->window_state_event = burro_app_window_state;

    gtk_widget_class_set_template_from_resource (gtkclass, window_ui);

    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, canvas);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, gears);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, tools_revealer);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, message_view);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, message_scrolled_window);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, repl_combobox);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, console_entry);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, vram_tree_view);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, update_rate_label);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, duty_cycle_label);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, debug_pause_button);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, debug_next_button);
    gtk_widget_class_bind_template_child (gtkclass, BurroAppWindow, debug_peek_tree_view);
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
    if (file)
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

SCM_DEFINE (G_console_write_icon,
            "console-write-icon", 1, 0, 0,
            (SCM name), "\
Writes the icon, given by its icon theme name, to the console.")
{
    char *c_icon_name = scm_to_utf8_string (name);
    console_write_icon (c_icon_name);
    free (c_icon_name);
    return SCM_BOOL_T;
}

SCM_DEFINE (G_debug_peek_append, "debug-peek-append", 3, 0, 0,
            (SCM label, SCM value, SCM stack), "")
{
    gint64 now = g_get_monotonic_time();
    char *timestr = g_strdup_printf("%.3f", 1.0e-6 * now);
    char *slabel = scm_to_utf8_string(label);
    char *svalue = scm_to_utf8_string(value);
    char *sstack = scm_to_utf8_string(stack);
    GtkTreeIter iter;
    gtk_list_store_insert (app_window_cur->debug_peek_list_store,
                           &iter,
                           0);
    
    gtk_list_store_set (app_window_cur->debug_peek_list_store,
                        &iter,
                        DEBUG_COLUMN_TIME, timestr,
                        DEBUG_COLUMN_NAME, slabel,
                        DEBUG_COLUMN_VALUE, svalue,
                        DEBUG_COLUMN_STACK, sstack,
                        -1);
    g_free (timestr);
    g_free (slabel);
    g_free (svalue);
    g_free (sstack);
    return SCM_UNSPECIFIED;
}

void
burro_app_win_init_guile_procedures ()
{
#include "burro_app_win.x"
    scm_c_export ("get-sandbox",
                  "console-write-bytevector",
                  "console-write-icon",
                  "set-title",
                  "debug-peek-append",
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

