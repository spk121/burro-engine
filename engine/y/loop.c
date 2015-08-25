#include "../x.h"
#include "draw.h"
#include "eng.h"
#include "loop.h"
#include "repl.h"
#include "pulseaudio.h"

const double UPDATE_RATE = 1.0 / 60.0;
const double REFRESH_RATE = 1.0 / 60.0;

GMainLoop *main_loop = NULL;
gboolean active_flag = FALSE;
gboolean initialized_flag = FALSE;
gboolean minimized_flag = FALSE;
gboolean quitting_flag = FALSE;
gboolean run_full_speed_flag = FALSE;
guint idle_event_source_id = -1;
unsigned main_screen_tick_id = -1;

extern GtkWidget *main_screen;

GTimer *timer = NULL;
int update_count = 0;
int draw_count = 0;
double before_update_time = 0.0;
double after_update_time = 0.0;
double before_draw_time = 0.0;
double after_draw_time = 0.0;
double thousand_frames_draw_time = 0.0;
double measured_frame_rate = 0.0;
double thousand_updates_time = 0.0;
double measured_updates_rate = 0.0;

// typedef int (*eng_delta_t_handler)(double delta_t);
/* typedef SCM (*eng_delta_t_handler)(SCM time); */
/* eng_delta_t_handler do_idle = NULL; */
/* eng_delta_t_handler do_after_draw_frame = NULL; */
SCM do_idle = SCM_UNSPECIFIED;
SCM do_after_draw_frame = SCM_UNSPECIFIED;

static gboolean idle_state_event_cb (void *dummy);
static int tick_cb (GtkWidget *widget, GdkFrameClock *frame_clock, void *user_data);
static void paint (void);

void
loop_set_full_speed_flag (void)
{
    run_full_speed_flag = TRUE;
}

void
loop_unset_full_speed_flag (void)
{
    run_full_speed_flag = false;
}

static void
loop_set_game_update_func (SCM idle)
{
  SCM var = scm_lookup (idle);
  if (!scm_is_true (var) || !scm_is_true (scm_variable_p (var)))
  {
      g_critical ("invalid game update func");
      return;
  }
  
  SCM ref = guile_variable_ref_safe (var);
  if (!scm_is_true (ref) || !scm_is_true (scm_procedure_p (ref)))
  {
      g_critical ("invalid game update func");
      return;
  }
    
    do_idle = ref;
}

static void
loop_set_after_draw_frame_func (SCM after_frame)
{
  SCM var = scm_lookup (after_frame);
  if (!scm_is_true (var) || !scm_is_true (scm_variable_p (var)))
  {
      g_critical ("invalid after frame func");
      return;
  }
  
  SCM ref = guile_variable_ref_safe (var);
  if (!scm_is_true (ref) || !scm_is_true (scm_procedure_p (ref)))
  {
      g_critical ("invalid after frame func");
      return;
  }
    
    do_after_draw_frame = ref;
}

void loop ()
{
    active_flag = TRUE;

    timer = xg_timer_new();
    g_timer_start (timer);
    update_count = 0;
    draw_count = 0;
    before_update_time = xg_timer_elapsed (timer);
    after_update_time = before_update_time;
    before_draw_time =  before_update_time;
    after_draw_time =  before_update_time;
    main_loop = xg_default_main_loop_new ();
    idle_event_source_id = g_timeout_add (1, idle_state_event_cb, NULL);
    main_screen_tick_id = gtk_widget_add_tick_callback (GTK_WIDGET(main_screen), tick_cb, NULL, NULL);

    initialized_flag = TRUE;

    xg_main_loop_run (main_loop);
    xg_main_loop_unref (main_loop);
}

void
loop_quit ()
{
    quitting_flag = TRUE;
    if (initialized_flag)
        xg_main_loop_quit (main_loop);
    else
        g_warning ("Main loop quits before it was initialized");
}

static gboolean idle_state_event_cb (void *dummy)
{
    double cur_time;

    if (quitting_flag)
        return FALSE;

    cur_time = xg_timer_elapsed (timer);

    if (initialized_flag && !minimized_flag)
    {
        if (active_flag)
        {
            //audio_time_cur = cur_time;
            if (run_full_speed_flag || ((cur_time - before_update_time) > UPDATE_RATE))
            {
                repl_tick ();
                if (do_idle != SCM_UNSPECIFIED)
                    scm_call_1 (do_idle, scm_from_double (cur_time));
              
                update_count ++;
                before_update_time = cur_time;
                after_update_time = xg_timer_elapsed (timer);

                if (update_count % 1000 == 0)
                {
                    measured_updates_rate 
                        = 1000.0 / (after_update_time - thousand_updates_time);
                    thousand_updates_time = after_update_time;
                    g_debug ("Update Rate: %f", measured_updates_rate);
                }
            }
        }
        else
        {
            //audio_pause ();
            // Figure out a way to sleep until the next gtk event comes in
        }
        /* if (!run_full_speed_flag) */
        /*     xg_usleep(10); */
    }

    return TRUE;
}

static int
tick_cb (GtkWidget *widget, GdkFrameClock *frame_clock, void *user_data)
{
    double cur_time;

    if (quitting_flag)
        return FALSE;

    // cur_time = xg_timer_elapsed (timer);
    cur_time = (double) gdk_frame_clock_get_frame_time (frame_clock) * 1.0e-6;

    if (initialized_flag && !minimized_flag)
    {
        if (active_flag)
        {
            if (run_full_speed_flag || ((cur_time - before_draw_time) > REFRESH_RATE))
            {
                paint ();
                if (do_after_draw_frame != SCM_UNSPECIFIED)
                    scm_call_1 (do_after_draw_frame, scm_from_double (after_draw_time));

                draw_count ++;
                before_draw_time = cur_time;
                after_draw_time = (double) gdk_frame_clock_get_frame_time (frame_clock) * 1.0e-6;

                if (draw_count % 1000 == 0)
                {
                    measured_frame_rate 
                        = 1000.0 / (after_draw_time - thousand_frames_draw_time);
                    thousand_frames_draw_time = after_draw_time;
                    g_debug ("Frame Rate: %f", measured_frame_rate);
                }
            }
            if (!run_full_speed_flag)
                xg_usleep (1000);
        }
        else
        {
            //audio_pause ();
            // Figure out a way to sleep until the next gtk event comes in
        }
    }

    return TRUE;
    
}

static void paint ()
{
    /* Have the engine render the backgrounds and objects to a bitmap */
    draw ();

    /* Have GTK draw the bitmap to the screen */
    eng_present ();
}

double
loop_time()
{
    return xg_timer_elapsed(timer);
}

////////////////////////////////////////////////////////////////


SCM_DEFINE (G_loop_set_full_speed, "loop-set-full-speed", 0, 0, 0, (void), "")
{
    loop_set_full_speed_flag ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_loop_unset_full_speed, "loop-unset-full-speed", 0, 0, 0, (void), "")
{
    loop_unset_full_speed_flag ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_loop_set_game_update_func, "loop-set-idle-callback", 1, 0, 0, (SCM proc), "")
{
    SCM old_proc = do_idle;
    loop_set_game_update_func (proc);
    return old_proc;
}

SCM_DEFINE (G_loop_set_after_draw_func, "loop-set-after-draw-callback", 1, 0, 0, (SCM proc), "")
{
    SCM old_proc = do_after_draw_frame;
    loop_set_after_draw_frame_func (proc);
    return old_proc;
}

SCM_DEFINE (G_loop_quit, "loop-quit", 0, 0, 0, (void), "")
{
    loop_quit ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_loop_time, "loop-time", 0, 0, 0, (void), "")
{
    return scm_from_double (loop_time ());
}

void
loop_init_guile_procedures (void)
{
#include "loop.x"
    scm_c_export ("loop-set-full-speed", "loop-unset-full-speed",
                  "loop-set-idle-callback", "loop-set-after-draw-callback",
                  "loop-quit", "loop-time",
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
