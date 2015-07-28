/*-----------------------------------------------------------------------------
        audio_model.h
        Copyright (C) 2014
                Michael L. Gran (spk121)

        GPL3+
-----------------------------------------------------------------------------*/
/*! \file audio_model.h
    \brief audio defines and functionality

    This is a simple audio stream model.  There are 16 channels of
    signed 16-bit PCM data.  Buffers for each channel hold PCM
    waveforms that will soon be sent to the output.

    A simple tone generator function produces notes of a given
    frequency and envelope.  This can be queued into one of the
    channels at a specific time, for music, or right away, for sound.
*/

#ifndef BURRO_AUDIO_H
#define BURRO_AUDIO_H

#include <stdint.h>

//! Samples per seconds of the audio engine
#define AUDIO_SAMPLE_RATE_IN_HZ (22050u)
//! The size of the audio buffer.  Consequently the maximum note length.
#define AUDIO_BUFFER_DURATION_IN_MILLISECONDS (5000u)
//! Number of samples in the audio buffer
#define AUDIO_BUFFER_SIZE \
  ((AUDIO_BUFFER_DURATION_IN_MILLISECONDS * AUDIO_SAMPLE_RATE_IN_HZ) / 1000u)

//! Number of independent channels
#define AUDIO_CHANNEL_COUNT 16
//! Maximum amplitude of PCM waveform per channel
#define AUDIO_CHANNEL_AMPLITUDE_MAX ((INT16_MAX / AUDIO_CHANNEL_COUNT) - 1)
#define AUDIO_CHANNEL_AMPLITUDE_MAX_F ((double)(AUDIO_CHANNEL_AMPLITUDE_MAX))
//! Minimum amplitude of a PCM waveform per channel
#define AUDIO_CHANNEL_AMPLITUDE_MIN (-AUDIO_CHANNEL_AMPLITUDE_MAX)

//! Holds the information of the audio model
typedef struct
{
  int16_t channels[AUDIO_CHANNEL_COUNT][AUDIO_BUFFER_SIZE];
  int16_t sum[AUDIO_BUFFER_SIZE];
  double start_time;
} audio_model_t;

/*! \brief Initializes the audio model
    \param now
        seconds since the program began
*/
void audio_model_initialize(double now);

/*! \brief Adds a tone or noise waveform to an audio channel.
    \param channel
        pre-mixed audio channel, 0 to AUDIO_CHANNEL_COUNT-1
    \param start_time
        seconds since program start when the sound is supposed to play, or 0.0 for 'now'
    \param D_attack
        duration of the attack of the ADSR envelope
    \param D_decay
        duration of the decay of the ADSR envelope
    \param D_sustain
        duration of the sustain of the ADSR envelope
    \param D_release
        duration of the release of the ADSR envelope
    \param F_initial
        initial frequency in Hz of the tone in Hz
    \param F_attack
        frequency of the tone after D_attack in Hz
    \param F_sustain
        frequency of the tone after D_decay in Hz
    \param F_release
        frequency of the tone after D_release in Hz
    \param A_attack
        amplitude after D_attack, 0.0 to 1.0
    \param A_sustain
        amplitude after D_decay, 0.0 to 1.0
    \param duty
        relative width of the upper half-period of an oscillation, 0.0 to 1.0
    \param noise
        0 => tone, 1 => noise
    \param waveform
        0 => square, 1 => sine
*/
void audio_model_add_tone(int channel, double start_time,
                          double D_attack, double D_decay, double D_sustain,
                          double D_release, double F_initial, double F_attack,
                          double F_sustain, double F_release, double A_attack,
                          double A_sustain, double duty, int noise,
                          int waveform);

/*! \brief Adds a resource waveform to an audio channel
    \param channel
        pre-mixed audio channel, 0 to AUDIO_CHANNEL_COUNT-1
    \param start_time
        seconds since program start when the sound is supposed to play, or
        0.0 for 'now'
    \param resource
        name of audio resource in the GResource bundle
*/
void audio_model_add_wave_from_resource (int channel, double start_time,
					 const char *resource);

/*! \brief Returns a pointer to the mixed waveform
 */
int16_t *audio_model_get_wave();

/*! \brief De-queues samples from the audio model
    \param n
        Number of samples to be removed
*/
void audio_model_dequeue(unsigned n);

void am_init_guile_procedures (void);
#endif
