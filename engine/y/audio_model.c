/*-----------------------------------------------------------------------------
  audio_model.c
  Copyright (C) 2014, 2015, 2018
  Michael L. Gran (spk121)

  GPL3+
  -----------------------------------------------------------------------------*/
#define _GNU_SOURCE
#include <math.h>
#include <stdalign.h>
#include <string.h>
#include "../x.h"
#include "audio_model.h"
#include "loop.h"
#include "rand.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-conversion"
#pragma GCC diagnostic ignored "-Wconversion"

/** A Guile SCM that holds the definition of the Guile instrument
    foreign object type. */
SCM G_instrument_tag;

/** Holds a list of sounds to be played.  Sounds are pairs where CAR
    is the start time and CDR is a native-endian, signed, 16-bit bytevector
    that holds a waveform. */
SCM_GLOBAL_VARIABLE_INIT (G_sound_playlist_var, "%sound-playlist", SCM_EOL);

/** Holds the information of the audio model */
static alignas(16) float am_working[AUDIO_BUFFER_SIZE];
static alignas(16) int16_t am_sum[AUDIO_BUFFER_SIZE];
static double am_timer_delta = 0.0;
static double am_timer = 0.0;

SCM_DEFINE (G_list_to_instrument, "list->instrument", 1, 0, 0,
            (SCM lst), "\
Given a 17-element list, this returns an instrument model.\n\
The contents of the list are\n\
attack duration, decay duration, release duration,\n\
initial freq adj, attack freq adj, sustain freq adj, release freq adj,\n\
attack amplitude adj, sustain ampl adj,\n\
noise flag, duty ratio,\n\
0th harmonic ampl adj through 6th harmonic ampl adj.")
{
    SCM_ASSERT_TYPE(scm_is_true (scm_list_p (lst)), lst, SCM_ARG1, "list->instrument",
                    "17 element list");
    instrument_t *inst = scm_gc_malloc (sizeof (instrument_t), "instrument");
#define DBL(n) scm_to_double(scm_list_ref(lst, scm_from_int(n)))
    inst->D_attack = CLAMP(DBL(0), 0.0, 5.0);
    inst->D_decay = CLAMP(DBL(1), 0.0, 5.0);
    inst->D_release = CLAMP(DBL(2), 0.0, 5.0);
    inst->F_initial = CLAMP(DBL(3), 0.0, 10.0);
    inst->F_attack = CLAMP(DBL(4), 0.0, 10.0);
    inst->F_sustain = CLAMP(DBL(5), 0.0, 10.0);
    inst->F_release = CLAMP(DBL(6), 0.0, 10.0);
    inst->A_attack = CLAMP(DBL(7), 0.0, 1.0);
    inst->A_sustain = CLAMP(DBL(8), 0.0, 1.0);
    inst->noise = scm_to_bool (scm_list_ref (lst, scm_from_int(9)));
    inst->duty = CLAMP(DBL(10), 0.01, 0.99);
    for (int i = 0; i < HARMONICS_NUM; i ++)
        inst->A_harmonics[i] = CLAMP(DBL(i+11), 0.0, 2.0);

    scm_remember_upto_here_1 (lst);
    return scm_make_foreign_object_1 (G_instrument_tag, inst);
}

SCM_DEFINE (G_instrument_generate_wave, "instrument-generate-wave",
            4, 0, 0, (SCM s_instrument, SCM s_frequency, SCM s_duration, SCM s_amplitude), "\
Renders a note into a Guile bytevector containing a native-endian,\n\
signed, 16-bit waveform sampled at AUDIO_SAMPLE_RATE_IN_HZ.  The\n\
rendering is according to an instrument model.")
{
    instrument_t *I = scm_foreign_object_ref (s_instrument, 0);
    double A = scm_to_double (s_amplitude);
    double D = scm_to_double (s_duration);
    double F = scm_to_double (s_frequency);

    double duration_in_seconds = MAX (D, I->D_attack + I->D_decay + I->D_release);
    double D_sustain = duration_in_seconds - (I->D_attack + I->D_decay + I->D_release);

    size_t duration_in_samples = ceil(duration_in_seconds * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    SCM vec = scm_c_make_bytevector (duration_in_samples * sizeof (int16_t));

    double t = 0.0;
    double  start_of_period_in_seconds = 0.0;
    double amplitude, frequency;
    double period = 0.0;
    double end_of_period_in_seconds;
    size_t sample_cur = 0;
    int first = TRUE;
    int level_a, level_b;
    static int count = 0;
    count ++;

    while (sample_cur < duration_in_samples)
    {
        // We're staring a new period in the waveform, so we compute
        // the current amplitude and frequency values.
        if(first || t >= end_of_period_in_seconds)
        {
            if (first)
                first = FALSE;
            else
                while (t - start_of_period_in_seconds >= period)
                    start_of_period_in_seconds += period;
            if (t < I->D_attack)
            {
                amplitude = A * ((I->A_attack / I->D_attack) * t);
                frequency = F * ((I->F_attack - I->F_initial) * t / I->D_attack + I->F_initial);
            }
            else if (t < I->D_attack + I->D_decay)
            {
                amplitude = A * (((I->A_sustain - I->A_attack) / I->D_decay) * (t - I->D_attack) + I->A_attack);
                frequency = F * (((I->F_sustain - I->F_attack) / I->D_decay) * (t - I->D_attack) + I->F_attack);
            }
            else if (t < I->D_attack + I->D_decay + D_sustain)
            {
                amplitude = A * I->A_sustain;
                frequency = F * I->F_sustain;
            }
            else if (t < I->D_attack + I->D_decay + D_sustain + I->D_release)
            {
                amplitude = A * ((-I->A_sustain / I->D_release) * (t - I->D_attack - I->D_decay - D_sustain) + I->A_sustain);
                frequency = F * (((I->F_release - I->F_sustain) / I->D_release) * (t - I->D_attack - I->D_decay - D_sustain) + I->F_sustain);
            }
            else
            {
                amplitude = 0.0;
                frequency = F * I->F_release;
            }

            period = 1.0 / frequency;
            end_of_period_in_seconds = start_of_period_in_seconds + period;
            if (I->noise)
            {
                // Level A is the amplitude of the 1st half-period.
                // Level B is the second half-period.  For "tone",
                // obviously it will be positive then negative.  The
                // noise generator uses a random amplitude sign.
                if(rand_int_range (0, 2))
                    level_a = AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
                else
                    level_a = -AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
                if(rand_int_range (0, 2))
                    level_b = AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
                else
                    level_b = -AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
            }
            else
            {
                level_a = AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
                level_b = -AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
            }

        }

        double x = 0.0;
        if (t - start_of_period_in_seconds < period * I->duty)
        {
            // The first half-period of the waveform
            double theta = M_PI * (t - start_of_period_in_seconds) / (period * I->duty);
            for (int i = 0; i < HARMONICS_NUM; i ++)
                x += level_a * sin((1.0 + i) * theta) * I->A_harmonics[i];
        }
        else if (t < end_of_period_in_seconds)
        {
            // The second half-period of the waveform.
            double theta = M_PI * ((t - start_of_period_in_seconds) - period * I->duty) / (period * (1.0 - I->duty));
            for (int i = 0; i < HARMONICS_NUM; i ++)
                x += level_b * sin((1.0 + i) * theta) * I->A_harmonics[i];
        }
        else
            g_assert_not_reached();

        scm_bytevector_s16_set_x (vec,
                                  scm_from_size_t (sample_cur * sizeof(int16_t)),
                                  scm_from_int16 (CLAMP ((int16_t)round(x), -AUDIO_CHANNEL_AMPLITUDE_MAX_F, AUDIO_CHANNEL_AMPLITUDE_MAX_F)),
                                  scm_native_endianness());

        sample_cur ++;
        t += 1.0 / (double) AUDIO_SAMPLE_RATE_IN_HZ;
    }
#if 1
    {
        if (count % 1 == 0) {
            FILE *fp;
            if(I->noise)
                fp = fopen("noise.txt", "wt");
            else
                fp = fopen("wave.txt", "wt");
            for(size_t i2 = 0; i2 < duration_in_samples; i2++)
                fprintf(fp, "%zu %d\n", i2,
                        (int) scm_bytevector_s16_ref (vec,
                                                      scm_from_size_t (i2 * sizeof(int16_t)),
                                                      scm_native_endianness()));

            fclose(fp);
        }
    }
#endif
    scm_remember_upto_here_1 (s_instrument);
    return vec;
}


SCM_DEFINE(G_play_wave, "play-wave", 1, 1, 0, (SCM wave, SCM start_time), "\
Play WAVE, a signed 16-bit bytevector of audio sampled at 44100 Hz.\n\
An optional start time may be provided, which is a Pulseaudio time in seconds\n\
If no start time is provided, the audio will play as soon as possible.")
{
    if (SCM_UNBNDP(start_time))
        start_time = scm_from_double (-1.0);
    SCM sound_playlist = scm_variable_ref (G_sound_playlist_var);
    sound_playlist = scm_append_x (scm_list_2 (sound_playlist,
                                               scm_list_1 (scm_list_3 (start_time, wave, scm_from_int(0)))));
    scm_variable_set_x (G_sound_playlist_var, sound_playlist);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_audio_time, "audio-time", 0, 0, 0, (void), "\
Return the estimated Pulseaudio timer, in seconds.")
{
    return scm_from_double (am_timer_delta + loop_time());
}

SCM_DEFINE (G_audio_last_update_time, "audio-last-update-time", 0, 0, 0, (void), "\
Return the Pulseaudio timer val of the last time Pulseaudio\n\
requested an update.")
{
    return scm_from_double (am_timer);
}

double
wave_time_start (SCM sound)
{
    return scm_to_double (scm_car (sound));
}

double
wave_time_end (SCM sound)
{
    return wave_time_start (sound)
        + ((double) scm_c_bytevector_length (scm_cadr (sound)) / sizeof(int16_t)) / AUDIO_SAMPLE_RATE_IN_HZ;
}

int16_t *wave_data (SCM sound)
{
    return (int16_t *) SCM_BYTEVECTOR_CONTENTS (scm_cadr (sound));
}

const int16_t *audio_model_get_wave(double time_start, size_t n)
{
    double time_end = time_start + (double) n / AUDIO_SAMPLE_RATE_IN_HZ;
    SCM am_wave_list = scm_variable_ref(G_sound_playlist_var);

    am_timer = time_start;
    am_timer_delta = time_start - loop_time();

    /* Drop waves that have already finished. */
    /* FIXME: could be more efficient by using more schemey CDR operations. */
    for (int id = scm_to_int (scm_length (am_wave_list)) - 1; id >= 0; id --)
    {
        SCM s_wave = scm_list_ref (am_wave_list, scm_from_int (id));
        if (wave_time_start(s_wave) < 0.0)
        {
            // This wave had a start time of zero, meaning it is to
            // start as soon as possible.  We set its start time to
            // now.
            scm_list_set_x (am_wave_list,
                            scm_from_int(id),
                            scm_list_3(scm_from_double (time_start), scm_cadr(s_wave), scm_from_int(0)));
        }
        else if (wave_time_end(s_wave) < time_start)
        {
            // Wave has expired.  Dequeue it.
            if (id > 0)
                scm_delq_x (s_wave, am_wave_list);
            else
                am_wave_list = scm_cdr (am_wave_list);
        }
    }

    scm_variable_set_x (G_sound_playlist_var, am_wave_list);

    memset (am_working, 0, AUDIO_BUFFER_SIZE * sizeof(float));

    for (int id = scm_to_int (scm_length (am_wave_list)) - 1; id >= 0; id --)
    {
        SCM s_wave = scm_list_ref (am_wave_list, scm_from_int (id));
        double wave_start = wave_time_start(s_wave);
        double wave_end = wave_time_end(s_wave);

        if (time_end < wave_start)
        {
            // hasn't started yet
        }
        else if (time_start <= wave_start)
        {
            // wave starts during this sample
            size_t delta = (wave_start - time_start) * AUDIO_SAMPLE_RATE_IN_HZ;
            size_t len   = (wave_end - wave_start) * AUDIO_SAMPLE_RATE_IN_HZ;
            if (len > n - delta)
                len = n - delta;
            int16_t *wav = wave_data(s_wave);
            for (size_t i = 0; i < len; i ++)
                am_working[i + delta] += (float) wav[i];
            // Store the number of samples processed
            //am_wave_list = scm_list_set_x (am_wave_list, scm_from_int (id),
            //                               scm_list_3 (scm_car (s_wave), scm_cadr(s_wave), scm_from_size_t (len)));
            scm_list_set_x (s_wave, scm_from_int(2), scm_from_size_t (len));
        }
        else
        {
            // wave has already started
            // size_t delta = (time_start - wave_start) * AUDIO_SAMPLE_RATE_IN_HZ;
            size_t delta = scm_to_size_t (scm_list_ref (s_wave, scm_from_int(2)));
            size_t len   = scm_c_bytevector_length (scm_cadr(s_wave)) / 2 - delta;
            if (len > n)
                len = n;
            int16_t *wav = wave_data(s_wave);
            for (size_t i = 0; i < len; i ++)
                am_working[i] += (float) wav[i + delta];
            // Store the number of samples processed
            scm_list_set_x (s_wave, scm_from_int(2), scm_from_size_t (len + delta));
        }
    }

    for (int i = 0; i < n; i ++)
        am_sum[i] = (int16_t) round(CLAMP(am_working[i],
                                          AUDIO_CHANNEL_AMPLITUDE_MIN,
                                          AUDIO_CHANNEL_AMPLITUDE_MAX));

    return am_sum;
}


void
am_init_guile_procedures (void)
{
    G_instrument_tag = scm_make_foreign_object_type (scm_from_utf8_symbol ("instrument"),
                                                   scm_list_1 (scm_from_utf8_symbol ("data")),
                                                   NULL);

#include "audio_model.x"
    scm_c_export ("%sound-playlist",
                  "list->instrument",
                  "instrument-generate-wave",
                  "play-wave",
                  "audio-time",
                  "audio-last-update-time",
                  NULL
        );
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

#pragma GCC diagnostic pop
