#include <math.h>
#include <glib.h>
#include "engine.h"
#include "eng_audio.h"
#include "pulseaudio.h"

static void generate_tone_data(double D_attack, double D_decay, double D_sustain, double D_release,
                               double F_initial, double F_attack, double F_sustain, double F_release,
                               double A_attack, double A_sustain,
                               double duty, _Bool noise,
                               int8_t **buffer, size_t *length);
void initialize_channel(int i);
static void noise_update(int i);
static void tone_update(int i);


////////////////////////////////////////////////////////////////

void initialize_audio(void)
{
    e.priv.audio_time_head = g_timer_elapsed (e.priv.timer, NULL);
    e.priv.song_start_time = g_timer_elapsed (e.priv.timer, NULL) + 1.0;
    pulse_initialize_audio();
}

/* Do one iteration of the audio processing loop */
void audio_update(void)
{
    int i;
    
    for(i = 0; i < TONE_COUNT; i++)
        tone_update(i);
    for(i = 0; i < NOISE_COUNT; i++)
        noise_update(i);
    pulse_update_audio();
}

static void generate_tone_data(double D_attack, double D_decay, double D_sustain, double D_release,
                               double F_initial, double F_attack, double F_sustain, double F_release,
                               double A_attack, double A_sustain,
                               double duty, _Bool noise,
                               int8_t **buffer, size_t *length)
{
    /* D = duration in sec
       F = frequency in Hz
       A = amplitude, from 0.0 to 1.0 */
	double t, t_start, amplitude, frequency, period;
    double duration;
    int i, first;
    int level_a, level_b;

    *buffer = NULL;
    *length = 0;

    g_return_if_fail (F_initial >= 12.0 && F_initial < 22050);
    g_return_if_fail (D_attack >= 0.0);
    
    duration = D_attack + D_decay + D_sustain + D_release;
    *length = ceil(duration * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    *buffer = g_new(uint8_t, *length);

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
                if(g_rand_boolean(e.priv.seed))
                    level_a = 127 * amplitude;
                else
                    level_a = -127 * amplitude;
                if(g_rand_boolean(e.priv.seed))
                    level_b = 127 * amplitude;
                else
                    level_b = -127 * amplitude;
            }
            else
            {
                level_a = 127 * amplitude;
                level_b = -127 * amplitude;
            }

        }
        if (t - t_start < period * duty)
            (*buffer)[i] = level_a;
        else if (t - t_start < period)
            (*buffer)[i] = level_b;
        i ++;
        t += 1.0 / (double) AUDIO_SAMPLE_RATE_IN_HZ;
    }
#if 0
    {
        FILE *fp = fopen("wav.txt", "wt");
        int i;
        for(i = 0; i < *length; i++)
            fprintf(fp, "%d %d\n", i, (int)(*buffer)[i]);
        fclose(fp);
    }
#endif    
}

static void tone_update(int channel)
{
    int8_t *buffer;
    size_t length;
    char *str;
    int i, delta;
    int n;
    int y;

    if(e.tone[channel].start_trigger == TRUE)
    {
        g_debug("Generate %f Hz tone on channel %d", e.tone[channel].initial_frequency, channel);
        generate_tone_data(e.tone[channel].attack_duration,
                           e.tone[channel].decay_duration,
                           e.tone[channel].sustain_duration,
                           e.tone[channel].release_duration,
                           e.tone[channel].initial_frequency,
                           e.tone[channel].attack_frequency,
                           e.tone[channel].sustain_frequency,
                           e.tone[channel].release_frequency,
                           e.tone[channel].attack_amplitude,
                           e.tone[channel].sustain_amplitude,
                           e.tone[channel].duty,
                           FALSE,
                           &buffer, &length);
        delta = AUDIO_LATENCY_REQUESTED_IN_SAMPLES;
        for(i = 0; i < length; i++)
        {
            e.priv.audio_buf[i + delta] += (int16_t) buffer[i];
            e.priv.audio_count[i + delta] += 1;
        }
        g_free(buffer);
        e.tone[channel].start_trigger = FALSE;
    }
}

static void noise_update(int channel)
{
    int8_t *buffer;
    size_t length;
    char *str;
    int i, delta;
    int n;
    int y;

    if(e.noise[channel].start_trigger == TRUE)
    {
        g_debug("Generate %f Hz noise on channel %d", e.noise[channel].initial_frequency, channel);
        generate_tone_data(e.noise[channel].attack_duration,
                           e.noise[channel].decay_duration,
                           e.noise[channel].sustain_duration,
                           e.noise[channel].release_duration,
                           e.noise[channel].initial_frequency,
                           e.noise[channel].attack_frequency,
                           e.noise[channel].sustain_frequency,
                           e.noise[channel].release_frequency,
                           e.noise[channel].attack_amplitude,
                           e.noise[channel].sustain_amplitude,
                           e.noise[channel].duty,
                           TRUE,
                           &buffer, &length);
        delta = AUDIO_LATENCY_REQUESTED_IN_SAMPLES;
        for(i = 0; i < length; i++)
        {
            e.priv.audio_buf[i + delta] += (int16_t) buffer[i];
            e.priv.audio_count[i + delta] += 1;
        }
        g_free(buffer);
        e.noise[channel].start_trigger = FALSE;
    }
}


#if 0
static void start_wave(int i, pa_stream **master_stream)
{
    uint8_t *buffer;
    size_t length;
    char *str;

    stop_wave(i);
    e.wave[i].start_trigger = FALSE;
    
    e.priv.audio_buf[WAVE(i)] = g_memdup(e.wave[i].wave, e.wave[i].count);
    e.priv.audio_pos[WAVE(i)] = 0;
    e.priv.audio_len[WAVE(i)] = e.wave[i].count;

    /* If the channel is waiting for data, ship it now. */
    if(e.priv.audio_waiting[WAVE(i)])
    {
        if (*master_stream == NULL)
            *master_stream = e.priv.audio_stream[WAVE(i)];
        write_to_stream(e.priv.audio_stream[WAVE(i)],
                        e.priv.audio_waiting[WAVE(i)],
                        WAVE(i),
                        *master_stream);
    }
}


static void stop_noise(int i)
{
    /* Flush the playback buffer of this stream */
    pa_stream_flush(e.priv.audio_stream[NOISE(i)], NULL, NULL);
    g_free(e.priv.audio_buf[NOISE(i)]);
    e.priv.audio_buf[NOISE(i)] = NULL;
    e.noise[i].stop_trigger = FALSE;
    e.noise[i].is_playing = FALSE;
}

static void stop_tone(int i)
{
    /* Flush the playback buffer of this stream */
    pa_stream_flush(e.priv.audio_stream[TONE(i)], NULL, NULL);
    g_free(e.priv.audio_buf[TONE(i)]);
    e.priv.audio_buf[TONE(i)] = NULL;
    e.tone[i].stop_trigger = FALSE;
    e.tone[i].is_playing = FALSE;
}

static void stop_wave(int i)
{
    /* Flush the playback buffer of this stream */
    pa_stream_flush(e.priv.audio_stream[WAVE(i)], NULL, NULL);
    g_free(e.priv.audio_buf[WAVE(i)]);
    e.priv.audio_buf[WAVE(i)] = NULL;
    e.tone[i].stop_trigger = FALSE;
    e.tone[i].is_playing = FALSE;
}
#endif

        
/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
