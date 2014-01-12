#include "../x/xglib.h"
#include "draw.h"
#include "eng.h"
#include "loop.h"

const double UPDATE_RATE = 1.0 / 60.0;
const double REFRESH_RATE = 1.0 / 60.0;

GMainLoop *main_loop = NULL;
gboolean active_flag = FALSE;
gboolean initialized_flag = FALSE;
gboolean minimized_flag = FALSE;
gboolean quitting_flag = FALSE;
gboolean run_full_speed_flag = FALSE;
guint idle_event_source_id = -1;

GTimer *timer = NULL;
int update_count = 0;
int draw_count = 0;
double before_update_time = 0.0;
double after_update_time = 0.0;
double before_draw_time = 0.0;
double after_draw_time = 0.0;
double hundred_frames_draw_time = 0.0;
double measured_frame_rate = 0.0;

typedef int (*eng_delta_t_handler)(double delta_t);

eng_delta_t_handler do_idle = NULL;
eng_delta_t_handler do_after_draw_frame = NULL;

static gboolean idle_state_event_cb (void *dummy);
static void paint (void);

void
loop_set_full_speed_flag (void)
{
  run_full_speed_flag = TRUE;
}

void loop_set_game_update_func (int idle (double delta_t))
{
  do_idle = idle;
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
  idle_event_source_id = xg_idle_add (idle_state_event_cb, NULL);

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
          //audio_update();
          if (run_full_speed_flag || ((cur_time - before_update_time) > UPDATE_RATE))
            {
              //if (do_idle != NULL)
              //  do_idle (cur_time - before_update_time);
                gchar *cmd = g_strdup_printf("(update %f %u)", cur_time, get_keyinput());
              xscm_c_eval_string(cmd);
              g_free(cmd);

              update_count ++;
              before_update_time = cur_time;
              after_update_time = xg_timer_elapsed (timer);

              if (run_full_speed_flag || ((cur_time - before_draw_time) > REFRESH_RATE))
                {
                  paint ();
                  if (do_after_draw_frame != NULL)
                    do_after_draw_frame (before_draw_time - after_draw_time);

                  draw_count ++;
                  before_draw_time = cur_time;
                  after_draw_time = xg_timer_elapsed (timer);

                  if (draw_count % 100 == 0)
                    {
                      measured_frame_rate 
                        = 100.0 / (after_draw_time - hundred_frames_draw_time);
                      hundred_frames_draw_time = after_draw_time;
                      g_debug ("Frame Rate: %f\n", measured_frame_rate);
                    }
                }
            }

          if (!run_full_speed_flag)
            xg_usleep (20);
        }
      else
        {
          //audio_pause ();
          // Figure out a way to sleep until the next gtk event comes in
          xg_usleep (2);
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

