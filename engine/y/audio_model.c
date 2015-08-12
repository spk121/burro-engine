/*-----------------------------------------------------------------------------
  audio_model.c
  Copyright (C) 2014, 2015
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


/** Holds the information of the audio model */
static alignas(16) int16_t am_channels[AUDIO_CHANNEL_COUNT][AUDIO_BUFFER_SIZE];
static alignas(16) float am_working[AUDIO_BUFFER_SIZE];
static alignas(16) int16_t am_sum[AUDIO_BUFFER_SIZE];
static double am_last_update_time = 0.0;

static void
generate_tone_data(double D_attack, double D_decay, double D_sustain,
                   double D_release, double F_initial, double F_attack,
                   double F_sustain, double F_release, double A_attack,
                   double A_sustain, double duty, _Bool noise, int waveform,
                   int16_t *buffer, size_t *length);
static void
update_sum();

void audio_model_initialize ()
{
    for (int i = 0; i < AUDIO_CHANNEL_COUNT; i ++)
        memset (am_channels[i], 0, sizeof (int16_t) * AUDIO_BUFFER_SIZE);
    am_last_update_time = loop_time ();
}

double audio_model_get_last_update_time()
{
    return am_last_update_time;
}

void audio_model_add_tone(int channel, double start_time,
                          double D_attack, double D_decay, double D_sustain,
                          double D_release, double F_initial, double F_attack,
                          double F_sustain, double F_release, double A_attack,
                          double A_sustain, double duty, int noise,
                          int waveform)
{
    size_t length;
    double now = loop_time();
    double time_since_last_update = now - am_last_update_time;
    double delta_t;

    if (start_time == 0.0)
        delta_t = 0.0;
    else
        delta_t = start_time - am_last_update_time;

    g_debug("Generate %f Hz tone on channel %d to start in %f s (%f sec since last update)",
            F_initial, channel, delta_t, time_since_last_update);

    int delta_t_in_samples = delta_t * AUDIO_SAMPLE_RATE_IN_HZ;
    length = AUDIO_BUFFER_SIZE - delta_t_in_samples;
    
    generate_tone_data(D_attack, D_decay, D_sustain, D_release,
                       F_initial, F_attack, F_sustain, F_release,
                       A_attack, A_sustain,
                       duty, noise, waveform,
                       &(am_channels[channel][delta_t_in_samples]), &length);

    update_sum();
}


#if 0
void audio_model_add_wave_from_resource (int channel, double start_time,
                                         const char *resource)
{
    GBytes *data = g_resources_lookup_data(path, 0, NULL);
    size_t length;
    int16_t *buffer;

    double now = loop_time();
    double time_since_last_update = now - am_last_update_time;
    double delta_t;
    int delta_i, i2, i;

    if (start_time == 0.0)
        delta_t = 0.0;
    else
        delta_t = start_time - am_last_update_time;
    g_debug("Play %s on channel %d to start in %f s (%f sec since las update)",
            path, channel, delta_t, time_since_last_update);

    length = g_bytes_get_size(data) / sizeof(int16_t);
    buffer = (int16_t *) g_bytes_get_data(data);

    delta_i = round(delta_t * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    for(i = 0; i < length; i++)
    {
        i2 = i + delta_i;
        if (i2 >= 0 && i2 < AUDIO_BUFFER_SIZE)
            audio_buf[channel][i2] = buffer[i];
    }
    g_bytes_unref(data);
    update_sum();
}
#endif

int16_t *audio_model_get_wave()
{
    return am_sum;
}

// Remove N samples from the model
void audio_model_dequeue(unsigned n)
{
    g_return_if_fail (AUDIO_BUFFER_SIZE >= n);
    
    int c;
    size_t samples_moved = AUDIO_BUFFER_SIZE - n;
    size_t bytes_moved = sizeof(int16_t) * samples_moved;
    size_t bytes_remaining = sizeof(int16_t) * n;

    for (c = 0; c < AUDIO_CHANNEL_COUNT; c ++)
    {
        memmove (&(am_channels[c][0]), &(am_channels[c][n]), bytes_moved);
        memset (&(am_channels[c][samples_moved]), 0, bytes_remaining); 
    }
    memmove (&(am_sum[0]), &(am_sum[n]), bytes_moved);
    memset (&(am_sum[samples_moved]), 0, bytes_remaining);
    am_last_update_time += (double) n / (double) AUDIO_SAMPLE_RATE_IN_HZ;
    // or should it be
    //   am_last_update_time = loop_time();
}

static void
generate_tone_data(double D_attack, double D_decay, double D_sustain,
                   double D_release, double F_initial, double F_attack,
                   double F_sustain, double F_release, double A_attack,
                   double A_sustain, double duty, _Bool noise, int waveform,
                   int16_t *buffer, size_t *length)
{
    /* D = duration in sec
       F = frequency in Hz
       A = amplitude, from 0.0 to 1.0 */
    double t, t_start, amplitude, frequency, period;
    double duration;
    size_t i;
    int first;
    int level_a, level_b;
    static int count = 0;
    count ++;
  
    duration = D_attack + D_decay + D_sustain + D_release;
    size_t tone_length = (size_t) ceil(duration * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    if (*length > tone_length)
        tone_length = *length;
 
    t = 0.0;
    t_start = 0.0;
    i = 0;
    period = 0.0;
    first = TRUE;
    while (i < tone_length)
    {
        if(first || t - t_start >= period)
        {
            if (first)
                first = FALSE;
            else
                while (t - t_start >= period)
                    t_start += period;
	  
            if (t < D_attack)
            {
                amplitude = (A_attack / D_attack) * t;
                frequency = ((F_attack - F_initial) / D_attack) *  t + F_initial;
            }
            else if (t < D_attack + D_decay)
            {
                amplitude = ((A_sustain - A_attack) / D_decay) * (t - D_attack) + A_attack;
                frequency = ((F_sustain - F_attack) / D_decay) * (t - D_attack) + F_attack;
            }
            else if (t < D_attack + D_decay + D_sustain)
            {
                amplitude = A_sustain;
                frequency = F_sustain;
            }
            else if (t < D_attack + D_decay + D_sustain + D_release)
            {
                amplitude = (-A_sustain / D_release) * (t - D_attack - D_decay - D_sustain) + A_sustain;
                frequency = ((F_release - F_sustain) / D_release) * (t - D_attack - D_decay - D_sustain) + F_sustain;
            }
            else
            {
                amplitude = 0;
                frequency = F_release;
            }
            period = 1.0 / frequency;
            if (noise)
            {
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
        if (t - t_start < period * duty)
        {
            if(waveform == 0)
                buffer[i] = level_a;
            else if (waveform == 1)
                buffer[i] = level_a * sin(M_PI * (t - t_start) / (period * duty));
        }
        else if (t - t_start < period)
        {
            if(waveform == 0)
                buffer[i] = level_b;
            else if (waveform == 1)
                buffer[i] = level_b * sin(M_PI * ((t - t_start) - period * duty) / (period * (1.0 - duty)));
        }
        i ++;
        t += 1.0 / (double) AUDIO_SAMPLE_RATE_IN_HZ;
    }
#if 1
    {
        if (count % 100 == 0) {
            FILE *fp;
            if(noise)
                fp = fopen("noise.txt", "wt");
            else
                fp = fopen("wave.txt", "wt");
            for(unsigned i2 = 0; i2 < *length; i2++)
                fprintf(fp, "%u %d\n", i2, (int)buffer[i2]);
            fclose(fp);
        }
    }
#endif    
}

// FIXME: this is pretty lazy.
static void
update_sum()
{
    int i, j;
    float min, max;
    memset (am_working, 0, AUDIO_BUFFER_SIZE * sizeof(int));
    for (j = 0; j < AUDIO_CHANNEL_COUNT; j ++)
    {
        for (i = 0; i < AUDIO_BUFFER_SIZE; i ++) {
            am_working[i] += (float) am_channels[j][i];
        }
    }
    
    min = (float) INT16_MAX;
    max = (float) INT16_MIN;
    for (i = 0; i < AUDIO_BUFFER_SIZE; i ++) {
        if (am_working[i] > max)
            max = am_working[i];
        if (am_working[i] < min)
            min = am_working[i];
    }
    if (max < -min)
        max = -min;

    /* I used 0x7ff0 instead of INT_MAX to avoid overflow issues */
    float scale = (float) 0x7ff0 / max;
    for (i = 0; i < AUDIO_BUFFER_SIZE; i ++) {
        am_sum[i] = (int16_t)(am_working[i] * scale);
    }
}

SCM_DEFINE (G_am_add_tone, "tone", 3, 0, 0, (SCM channel, SCM start_time, SCM tone), "")
{
    audio_model_add_tone (scm_to_int (channel),
                          scm_to_double (start_time),
                          // D_attack, D_decay, D_sustain, D_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (0))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (1))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (3))),
                          //F_initial, F_attack, F_sustain, F_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (4))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (5))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (6))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (7))),
                          // A_attack, A_sustain
                          scm_to_double (scm_list_ref (tone, scm_from_int (8))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (9))),
                          // Duty
                          scm_to_double (scm_list_ref (tone, scm_from_int (10))),
                          // noise, waveform
                          false,
                          scm_to_int (scm_list_ref (tone, scm_from_int (11))));
    return SCM_UNSPECIFIED;
}


SCM_DEFINE (G_am_add_simple_tone, "simple-tone", 3, 0, 0, (SCM channel, SCM start_time, SCM tone), "")
{
    audio_model_add_tone (scm_to_int (channel),
                          scm_to_double (start_time),
                          // D_attack, D_decay, D_sustain, D_release
                          0.01,
                          scm_to_double (scm_list_ref (tone, scm_from_int (0))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (1))),
                          0.01,
                          //F_initial, F_attack, F_sustain, F_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          // A_attack, A_sustain
                          scm_to_double (scm_list_ref (tone, scm_from_int (3))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (4))),
                          // Duty
                          0.5,
                          // noise, waveform
                          false, 0);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_am_add_noise, "noise", 3, 0, 0, (SCM channel, SCM start_time, SCM tone), "")
{
    audio_model_add_tone (scm_to_int (channel),
                          scm_to_double (start_time),
                          // D_attack, D_decay, D_sustain, D_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (0))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (1))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (3))),
                          //F_initial, F_attack, F_sustain, F_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (4))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (5))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (6))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (7))),
                          // A_attack, A_sustain
                          scm_to_double (scm_list_ref (tone, scm_from_int (8))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (9))),
                          // Duty
                          scm_to_double (scm_list_ref (tone, scm_from_int (10))),
                          // noise, waveform
                          true,
                          scm_to_int (scm_list_ref (tone, scm_from_int (11))));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_am_add_simple_noise, "simple-noise", 3, 0, 0, (SCM channel, SCM start_time, SCM tone), "")
{
    audio_model_add_tone (scm_to_int (channel),
                          scm_to_double (start_time),
                          // D_attack, D_decay, D_sustain, D_release
                          0.01,
                          scm_to_double (scm_list_ref (tone, scm_from_int (0))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (1))),
                          0.01,
                          //F_initial, F_attack, F_sustain, F_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          // A_attack, A_sustain
                          scm_to_double (scm_list_ref (tone, scm_from_int (3))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (4))),
                          // Duty
                          0.5,
                          // noise, waveform
                          true, 0);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_beep, "beep", 0, 0, 0, (void), "")
{
    audio_model_add_tone (0,
                          0.0,
                          // D_attack, D_decay, D_sustain, D_release
                          0.00, 0.1, 0.1, 0.1,
                          //F_initial, F_attack, F_sustain, F_release
                          440.0, 430.0, 420.0, 410.0,
                          // A_attack, A_sustain
                          1.0, 0.8,
                          // Duty
                          0.5,
                          // noise, waveform
                          false, 0);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_wave, "wave", 3, 0, 0, (SCM channel, SCM now, SCM index),"\
Copy wave resource INDEX into CHANNEL at a specific clock time")
{
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_audio_last_update_time, "audio-last-update-time", 0, 0, 0, (void), "\
Return the loop timestamp of the last time that the mixed audio was sent\n\
to the sound server.")
{
    return scm_from_double(am_last_update_time);
}

SCM_DEFINE (G_audio_sample_index, "audio-time->index", 1, 0, 0, (SCM now), "\
For music applications, this returns the index into a channel buffer that represents\n\
a specific clock time.")
{
    return scm_from_int((int)((scm_to_double(now) - am_last_update_time) * AUDIO_SAMPLE_RATE_IN_HZ));
}

#define SCM_BV(_bank) \
    scm_pointer_to_bytevector(scm_from_pointer(am_channels[_bank], NULL), scm_from_int (AUDIO_BUFFER_SIZE), scm_from_int (0), scm_from_int(SCM_ARRAY_ELEMENT_TYPE_S16))

/* SCM_VARIABLE_INIT (G_channel_1_bv, "channel-1-bv", SCM_BV(0)); */
/* SCM_VARIABLE_INIT (G_channel_2_bv, "channel-2-bv", SCM_BV(1)); */
/* SCM_VARIABLE_INIT (G_channel_3_bv, "channel-3-bv", SCM_BV(2)); */
/* SCM_VARIABLE_INIT (G_channel_4_bv, "channel-4-bv", SCM_BV(3)); */
/* SCM_VARIABLE_INIT (G_channel_5_bv, "channel-5-bv", SCM_BV(4)); */
/* SCM_VARIABLE_INIT (G_channel_6_bv, "channel-6-bv", SCM_BV(5)); */
/* SCM_VARIABLE_INIT (G_channel_7_bv, "channel-7-bv", SCM_BV(6)); */
/* SCM_VARIABLE_INIT (G_channel_8_bv, "channel-8-bv", SCM_BV(7)); */
/* SCM_VARIABLE_INIT (G_channel_9_bv, "channel-9-bv", SCM_BV(8)); */
/* SCM_VARIABLE_INIT (G_channel_10_bv, "channel-10-bv", SCM_BV(9)); */
/* SCM_VARIABLE_INIT (G_channel_11_bv, "channel-11-bv", SCM_BV(10)); */
/* SCM_VARIABLE_INIT (G_channel_12_bv, "channel-12-bv", SCM_BV(11)); */
/* SCM_VARIABLE_INIT (G_channel_13_bv, "channel-13-bv", SCM_BV(12)); */
/* SCM_VARIABLE_INIT (G_channel_14_bv, "channel-14-bv", SCM_BV(13)); */
/* SCM_VARIABLE_INIT (G_channel_15_bv, "channel-15-bv", SCM_BV(14)); */
/* SCM_VARIABLE_INIT (G_channel_16_bv, "channel-16-bv", SCM_BV(15)); */
#undef SCM_BV

void
am_init_guile_procedures (void)
{
#include "audio_model.x"
  scm_c_export ("tone",
                "simple-tone",
                "noise",
                "simple-noise",
                "beep",
                "wave",
                "channel-bytevector",
                "audio-last-update-time",
                "audio-time->index",
                "channel-1-bv",
                "channel-2-bv",
                "channel-3-bv",
                "channel-4-bv",
                "channel-5-bv",
                "channel-6-bv",
                "channel-7-bv",
                "channel-8-bv",
                "channel-9-bv",
                "channel-10-bv",
                "channel-11-bv",
                "channel-12-bv",
                "channel-13-bv",
                "channel-14-bv",
                "channel-15-bv",
                "channel-16-bv",
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

#pragma GCC diagnostic pop
