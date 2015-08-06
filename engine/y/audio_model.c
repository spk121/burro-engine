/*-----------------------------------------------------------------------------
  audio_model.h
  Copyright (C) 2014
  Michael L. Gran (spk121)

  GPL3+
  -----------------------------------------------------------------------------*/
#define _GNU_SOURCE
#include <math.h>
#include <string.h>
#include "../x.h"
#include "audio_model.h"
#include "loop.h"
#include "rand.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-conversion"
#pragma GCC diagnostic ignored "-Wconversion"

audio_model_t am;
static int am_initialized = 0;
static void
generate_tone_data(double D_attack, double D_decay, double D_sustain,
                   double D_release, double F_initial, double F_attack,
                   double F_sustain, double F_release, double A_attack,
                   double A_sustain, double duty, _Bool noise, int waveform,
                   int16_t **buffer, size_t *length);
static void
update_sum();

void audio_model_initialize(double now)
{
    memset((void *) &am, 0, sizeof(am));
    am.start_time = now;
    am_initialized = 1;
}

void audio_model_add_tone(int channel, double start_time,
                          double D_attack, double D_decay, double D_sustain,
                          double D_release, double F_initial, double F_attack,
                          double F_sustain, double F_release, double A_attack,
                          double A_sustain, double duty, int noise,
                          int waveform)
{
    int16_t *buffer;
    size_t length;
    double now = loop_time();
    double time_since_last_update = now - am.start_time;
    double delta_t;
    int delta_i, i2, i;

    if (start_time == 0.0)
    {
        delta_t = 0.0;
        g_debug("Generate %f Hz tone on channel %d to start in %f s (%f sec since las update)",
                F_initial, channel, delta_t, time_since_last_update);
    }
    else
    {
        delta_t = start_time - am.start_time;
        g_debug("Generate %f Hz tone on channel %d to start in %f s (%f sec since las update)",
                F_initial, channel, delta_t, time_since_last_update);
    }
        
    generate_tone_data(D_attack, D_decay, D_sustain, D_release,
                       F_initial, F_attack, F_sustain, F_release,
                       A_attack, A_sustain,
                       duty, noise, waveform,
                       &buffer, &length);

    delta_i = (int) round(delta_t * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    for(i = 0; i < length; i++)
    {
        i2 = i + delta_i;
        if (i2 >= 0 && i2 < AUDIO_BUFFER_SIZE)
            am.channels[channel][i2] = buffer[i];
    }
    g_free(buffer);
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
    double time_since_last_update = now - am->start_time;
    double delta_t;
    int delta_i, i2, i;

    if (start_time == 0.0)
    {
        delta_t = 0.0;
        g_debug("Play '%s' on channel %d to start in %f s (%f sec since las update)",
                path, channel, delta_t, time_since_last_update);
    }
    else
    {
        delta_t = start_time - am->start_time;
        g_debug("Play %s on channel %d to start in %f s (%f sec since las update)",
                path, channel, delta_t, time_since_last_update);
    }
        

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
    g_return_val_if_fail (am_initialized != 0, NULL);
    return am.sum;
}

// Remove N samples from the model
void audio_model_dequeue(unsigned n)
{
    g_return_if_fail (AUDIO_BUFFER_SIZE >= n);
    g_return_if_fail (am_initialized != 0);
    
    int c;
    size_t samples_moved = AUDIO_BUFFER_SIZE - n;
    size_t bytes_moved = sizeof(int16_t) * samples_moved;
    size_t bytes_remaining = sizeof(int16_t) * n;

    for (c = 0; c < AUDIO_CHANNEL_COUNT; c ++)
    {
        memmove (&(am.channels[c][0]), &(am.channels[c][n]), bytes_moved);
        memset (&(am.channels[c][samples_moved]), 0, bytes_remaining); 
    }
    memmove (&(am.sum[0]), &(am.sum[n]), bytes_moved);
    memset (&(am.sum[samples_moved]), 0, bytes_remaining);
    am.start_time += (double) n / (double) AUDIO_SAMPLE_RATE_IN_HZ;
}

static void
generate_tone_data(double D_attack, double D_decay, double D_sustain,
                   double D_release, double F_initial, double F_attack,
                   double F_sustain, double F_release, double A_attack,
                   double A_sustain, double duty, _Bool noise, int waveform,
                   int16_t **buffer, size_t *length)
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
  
    *buffer = NULL;
    *length = 0;

    duration = D_attack + D_decay + D_sustain + D_release;
    *length = (size_t) ceil(duration * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    *buffer = g_new(int16_t, *length);

    t = 0.0;
    t_start = 0.0;
    i = 0;
    period = 0.0;
    first = TRUE;
    while (i < *length)
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
                (*buffer)[i] = level_a;
            else if (waveform == 1)
                (*buffer)[i] = level_a * sin(M_PI * (t - t_start) / (period * duty));
        }
        else if (t - t_start < period)
        {
            if(waveform == 0)
                (*buffer)[i] = level_b;
            else if (waveform == 1)
                (*buffer)[i] = level_b * sin(M_PI * ((t - t_start) - period * duty) / (period * (1.0 - duty)));
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
                fprintf(fp, "%u %d\n", i2, (int)(*buffer)[i2]);
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
    memset (am.sum, 0, AUDIO_BUFFER_SIZE * sizeof(int16_t));
    for (j = 0; j < AUDIO_CHANNEL_COUNT; j ++)
        for (i = 0; i < AUDIO_BUFFER_SIZE; i ++) {
            am.sum[i] += am.channels[j][i];
        }
}

SCM_DEFINE (G_am_add_tone, "am-add-tone", 3, 0, 0, (SCM channel, SCM start_time, SCM tone), "")
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
                          scm_to_int (scm_list_ref (tone, scm_from_int (11))),
                          scm_to_int (scm_list_ref (tone, scm_from_int (12))));
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
                          0, 0);
    return SCM_UNSPECIFIED;
}


void
am_init_guile_procedures (void)
{
#include "audio_model.x"
  scm_c_export ("enqueue-tone",
                "enqueue-simple-tone",
                "enqueue-noise",
                "enqueue-simple-noise",
                "enqueue-wave",
                "wave-add-from-file",
                "channel-bytevector",
                "wave-bytevector",
                "beep",
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
