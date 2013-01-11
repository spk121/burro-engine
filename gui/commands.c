#include <string.h>
#include "extern.h"
#include "commands.h"

GScanner *m_scanner;

void
initialize_command_parser()
{
    m_scanner = g_scanner_new (NULL);
    m_scanner->input_name = "Game Commands"
}

void do_commands_from_string (gchar *str, gsize length)
{
    g_scanner_input_text (m_scanner, str, length);
    while (1)
    {
        gchar *id = xg_scanner_get_next_identifier (m_scanner);
        if (g_str_equal (id, "NOOP"))
            cmd_noop();
        else if (g_str_equal (id, "BLANK"))
            cmd_blank (xg_scanner_get_next_token_int (m_scanner));
        else if (g_str_equal (id, "COLORSWAP"))
            cmd_color_swap (xg_scanner_get_next_token_int (m_scanner));
        else if (g_str_equal (id, "BRIGHTNESS"))
            cmd_brightness (xg_scanner_get_next_token_float (m_scanner));
        else if (g_str_equal (id, "BGCOLOR"))
            cmd_bg_color (xg_scanner_get_next_token_hex (m_scanner));
        else if (g_str_equal (id, "BG"))
            cmd_bg (xg_scanner_get_next_token_int (m_scanner), /* BG # */
                    xg_scanner_get_next_token_int (m_scanner), /* enable */
                    xg_scanner_get_next_token_int (m_scanner), /* priority */
                    xg_scanner_get_next_token_float (m_scanner), /* center x */
                    xg_scanner_get_next_token_float (m_scanner), /* center y */
                    xg_scanner_get_next_token_int (m_scanner), /* center i */
                    xg_scanner_get_next_token_int (m_scanner), /* center j */
                    xg_scanner_get_next_token_float (m_scanner), /* expansion */
                    xg_scanner_get_next_token_float (m_scanner) /* rotation */
                );
        else if (g_str_equal (id, "SPRITE"))
            cmd_sprite (xg_scanner_get_next_token_int (m_scanner), /* OBJ # */
                        xg_scanner_get_next_token_int (m_scanner), /* enable */
                        xg_scanner_get_next_token_int (m_scanner), /* priority */
                        xg_scanner_get_next_token_int (m_scanner), /* spritesheet_i */
                        xg_scanner_get_next_token_int (m_scanner), /* spritesheet_j */
                        xg_scanner_get_next_token_int (m_scanner), /* sprite_width_i */
                        xg_scanner_get_next_token_int (m_scanner), /* sprite_width_j */
                        xg_scanner_get_next_token_float (m_scanner), /* center x */
                        xg_scanner_get_next_token_float (m_scanner), /* center y */
                        xg_scanner_get_next_token_int (m_scanner), /* center i */
                        xg_scanner_get_next_token_int (m_scanner), /* center j */
                        xg_scanner_get_next_token_float (m_scanner), /* expansion */
                        xg_scanner_get_next_token_float (m_scanner) /* rotation */
                );
        else if (g_str_equal (id, "TONE"))
            cmd_tone (xg_scanner_get_next_token_int (m_scanner), /* Channel # */
                      xg_scanner_get_next_token_int (m_scanner), /* Waveform */
                      xg_scanner_get_next_token_float (m_scanner), /* attack_duration */
                      xg_scanner_get_next_token_float (m_scanner), /* decay duration */
                      xg_scanner_get_next_token_float (m_scanner), /* sustain duration */
                      xg_scanner_get_next_token_float (m_scanner), /* release duration */
                      xg_scanner_get_next_token_float (m_scanner), /* initial frequency */
                      xg_scanner_get_next_token_float (m_scanner), /* attack frequency */
                      xg_scanner_get_next_token_float (m_scanner), /* sustain frequency */
                      xg_scanner_get_next_token_float (m_scanner), /* release frequency */
                      xg_scanner_get_next_token_float (m_scanner), /* attack amplitude */
                      xg_scanner_get_next_token_float (m_scanner), /* sustain amplitude */
                      xg_scanner_get_next_token_float (m_scanner) /* duty */
                );
        else if (g_str_equal (id, "NOISE"))
            cmd_tone (xg_scanner_get_next_token_int (m_scanner), /* Channel # */
                      xg_scanner_get_next_token_int (m_scanner), /* Waveform */
                      xg_scanner_get_next_token_float (m_scanner), /* attack_duration */
                      xg_scanner_get_next_token_float (m_scanner), /* decay duration */
                      xg_scanner_get_next_token_float (m_scanner), /* sustain duration */
                      xg_scanner_get_next_token_float (m_scanner), /* release duration */
                      xg_scanner_get_next_token_float (m_scanner), /* initial frequency */
                      xg_scanner_get_next_token_float (m_scanner), /* attack frequency */
                      xg_scanner_get_next_token_float (m_scanner), /* sustain frequency */
                      xg_scanner_get_next_token_float (m_scanner), /* release frequency */
                      xg_scanner_get_next_token_float (m_scanner), /* attack amplitude */
                      xg_scanner_get_next_token_float (m_scanner), /* sustain amplitude */
                      xg_scanner_get_next_token_float (m_scanner) /* duty */
                );
        else if (g_str_equal (id, "WAV"))
            cmd_wave (xg_scanner_get_next_token_int (m_scanner), /* OBJ # */
                      xg_scanner_get_next_token_int (m_scanner), /* enable */
                      xg_scanner_get_next_token_float (m_scanner) /* volume */
                );
        else if (g_str_equal (id, "LOAD_BG_MAP"))
            cmd_load_bg_map (xg_scanner_get_next_token_int (m_scanner), /* BG # */
                    xg_scanner_get_next_token_string (m_scanner) /* map name */
                );
        else if (g_str_equal (id, "LOAD_BG_TILESHEET"))
            cmd_load_bg_map (xg_scanner_get_next_token_int (m_scanner), /* BG # */
                    xg_scanner_get_next_token_string (m_scanner) /* tilesheet name */
                );
        else if (g_str_equal (id, "LOAD_BG_BMP8"))
            cmd_load_bg_bmp8 (xg_scanner_get_next_token_int (m_scanner), /* BG # */
                    xg_scanner_get_next_token_string (m_scanner) /* bmp name */
                );
        else if (g_str_equal (id, "LOAD_BG_BMP16"))
            cmd_load_bg_bmp16 (xg_scanner_get_next_token_int (m_scanner), /* BG # */
                    xg_scanner_get_next_token_string (m_scanner) /* bmp name */
                );
        else if (g_str_equal (id, "LOAD_FG_SPRITESHEET"))
            cmd_load_spritesheet (xg_scanner_get_next_token_int (m_scanner), /* FG # */
                    xg_scanner_get_next_token_string (m_scanner) /* spritesheet name */
                );
        else if (g_str_equal (id, "LOAD_WAV"))
            cmd_load_wave (xg_scanner_get_next_token_int (m_scanner), /* WAVE BUFFER # */,
                           xg_scanner_get_next_token_string (m_scanner) /* wave name */
                );
        else
            xerror ("unknown command %s", id);

    }
}

static void
cmd_noop (void)
{
}

static void
cmd_blank (gboolean flag)
{
    e.blank = flag;
}

static void
cmd_colorswap (gboolean flag)
{
    e.color_swap = flag;
}

static void
cmd_brightness (gdouble brightness)
{
    if (brightness < 0.0 || brightness > 1.0)
        g_critical ("%s: brightness is out of range %f", __func__, brightness);
    e.brightness = brightness;
}

static void
cmd_bg_color (guint32 colorval)
{
    e.bg_color = colorval;
}

static void
cmd_bg (guint bg_index, gboolean enable, gint priority,
        gdouble center_x, gdouble center_y,
        gint center_i, gint center_j,
        gdouble expansion, gdouble rotation)
{
    bg_entry_t *bg;
    if (bg_index >= 0 && bg_index < MAIN_BACKGROUNDS_COUNT)
        bg = main_bg[bg_index];
    else if ((bg_index >= MAIN_BACKGROUNDS_COUNT) 
             && (bg_index < MAIN_BACKGROUNDS_COUNT + SUB_BACKGROUNDS_COUNT))
        bg = sub_bg[bg_index - MAIN_BACKGROUNDS_COUNT];
    else
    {
        g_critical ("%s: bg_index out of range", __func__, bg_index);
        return;
    }
    bg->enable = enable;
    if (priority >= 0 && priority < PRIORITY_COUNT)
        bg->priority = priority;
    else
        g_critical ("%s: priority out of range", __func__, priority);
    bg->center_x = center_x;
    bg->center_y = center_y;
    bg->center_i = center_i;
    bg->center_j = center_j;
    if (expansion > 0.0)
        bg->expansion = expansion;
    else
        g_critical ("%s: expansion out of range", __func__, expansion);
    bg->rotation = rotation;
}

static void
cmd_obj (guint obj_index, gboolean enable, gint priority,
        gdouble center_x, gdouble center_y,
        gint center_i, gint center_j,
        gdouble expansion, gdouble rotation)
{
    obj_entry_t *obj;
    if (obj_index >= 0 && obj_index < MAIN_SPRITES_COUNT)
        obj = main_obj[obj_index];
    else if ((obj_index >= MAIN_SPRITES_COUNT) 
             && (obj_index < MAIN_SPRITES_COUNT + SUB_SPRITES_COUNT))
        obj = sub_obj[obj_index - MAIN_SPRITES_COUNT];
    else
    {
        g_critical ("%s: obj_index out of range", __func__, obj_index);
        return;
    }
    obj->enable = enable;
    if (priority >= 0 && priority < PRIORITY_COUNT)
        obj->priority = priority;
    else
        g_critical ("%s: priority out of range", __func__, priority);
    if (spritesheet_i >= 0 && spritesheet_i < OBJSHEET_WIDTH_IN_PIXELS)
        obj->spritesheet_i = spritesheet_i;
    else
        g_critical ("%s: spritesheet_i out of range", __func__, spritesheet_i);
    if (spritesheet_j >= 0 && spritesheet_j < OBJSHEET_HEIGHT_IN_PIXELS)
        obj->spritesheet_j = spritesheet_j;
    else
        g_critical ("%s: spritesheet_j out of range", __func__, spritesheet_i);
    obj->sprite_width_i = sprite_width_i;
    obj->sprite_width_j = sprite_width_j
    obj->center_x = center_x;
    obj->center_y = center_y;
    obj->center_i = center_i;
    obj->center_j = center_j;
    if (expansion > 0.0)
        obj->expansion = expansion;
    else
        g_critical ("%s: expansion out of range", __func__, expansion);
    obj->rotation = rotation;
}

static void
cmd_tone (guint channel, guint waveform, gdouble attack_duration, gdouble decay_duration, 
          gdouble release_duration, gdouble sustain_duration, gdouble initial_frequency,
          gdouble attack_frequency, gdouble sustain_frequency, gdouble release_frequency,
          gdouble attack_amplitude, gdouble sustain_amplitude, gdouble duty)
{
    tone_entry_t *t = &(e.tone[channel]);
    t->waveform = waveform;
    t->attack_duration = attack_duration;
    t->decay_duration = decay_duration;
    t->release_duration = release_duration;
    t->sustain_duration = sustain_duration;
    t->initial_frequency = initial_frequency;
    t->attack_frequency = attack_frequency;
    t->sustain_frequency = sustain_frequency;
    t->release_frequency = release_frequency;
    t->attack_amplitude = attack_amplitude;
    t->sustain_amplitude = sustain_amplitude;
    t->duty = duty;
}

static void
cmd_noise (guint channel, guint waveform, gdouble attack_duration, gdouble decay_duration, 
          gdouble release_duration, gdouble sustain_duration, gdouble initial_frequency,
          gdouble attack_frequency, gdouble sustain_frequency, gdouble release_frequency,
          gdouble attack_amplitude, gdouble sustain_amplitude, gdouble duty)
{
    noise_entry_t *t = &(e.noise[channel]);
    t->waveform = waveform;
    t->attack_duration = attack_duration;
    t->decay_duration = decay_duration;
    t->release_duration = release_duration;
    t->sustain_duration = sustain_duration;
    t->initial_frequency = initial_frequency;
    t->attack_frequency = attack_frequency;
    t->sustain_frequency = sustain_frequency;
    t->release_frequency = release_frequency;
    t->attack_amplitude = attack_amplitude;
    t->sustain_amplitude = sustain_amplitude;
    t->duty = duty;
}

static void
cmd_wave (int obj, gboolean enable, gdouble volume)
{
}

static void
cmd_load_bg_map (guint bg_index, char *name)
{

}

static void
cmd_load_bg_bmp8 (guint bg_index, char *name)
{

}

static void
cmd_load_bg_bmp32 (guint bg_index, char *name)
{

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

