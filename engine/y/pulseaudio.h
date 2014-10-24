#ifndef BURRO_PULSEAUDIO_H
#define BURRO_PULSEAUDIO_H

#include <stdint.h>

#define TONE_COUNT (8)
#define WAVE_COUNT (6)
#define NOISE_COUNT (2)
#define CHANNEL_COUNT (TONE_COUNT+NOISE_COUNT+WAVE_COUNT)

#define MICROSECONDS_PER_MILLISECOND (1000)

#define AUDIO_SAMPLE_RATE_IN_HZ (22050)
#define AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS (100)
#define AUDIO_LATENCY_REQUESTED_IN_SAMPLES ((AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS * AUDIO_SAMPLE_RATE_IN_HZ) / 1000)
#define AUDIO_LATENCY_MAX_IN_MILLISECONDS (1000)
#define AUDIO_LATENCY_MAX_IN_SAMPLES ((AUDIO_LATENCY_MAX_IN_MILLISECONDS * AUDIO_SAMPLE_RATE_IN_HZ) / 1000)
#define AUDIO_WAVEFORM_DURATION_MAX_IN_MILLISECONDS (4000)
#define AUDIO_WAVEFORM_DURATION_MAX_IN_SAMPLES ((AUDIO_WAVEFORM_DURATION_MAX_IN_MILLISECONDS * AUDIO_SAMPLE_RATE_IN_HZ) / 1000)
#define AUDIO_BUFFER_DURATION_MAX_IN_MILLISEC (AUDIO_WAVEFORM_DURATION_MAX_IN_MILLISECONDS + AUDIO_LATENCY_MAX_IN_MILLISECONDS)
#define AUDIO_BUFFER_SIZE ((AUDIO_BUFFER_DURATION_MAX_IN_MILLISEC * AUDIO_SAMPLE_RATE_IN_HZ) / 1000)

/* Set up the audio backend. */
void pulse_initialize_audio(void);

/* Do audio actions.  Return number of samples sent. */
void pulse_update_audio(void);

/* Tear down the audio backend. */
void pulse_finalize_audio(void);

void tone(int channel, double start_time,
          double D_attack, double D_decay, double D_sustain, double D_release,
          double F_initial, double F_attack, double F_sustain, double F_release,
          double A_attack, double A_sustain,
          double duty, int noise, int waveform);

void pulse_mainloop(void);


#endif
