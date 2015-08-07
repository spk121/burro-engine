/*-----------------------------------------------------------------------------
        audio_model.h
        Copyright (C) 2014
                Michael L. Gran (spk121)

        GPL3+
-----------------------------------------------------------------------------*/
/** \file audio_model.h
     audio defines and functionality

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

/** The Nintendo DS had 16 channels.  Audio was resampled down
 *  to 10-bit 32kHz Stereo before it was sent to the hardware
 *  for playback.
 *
 *  On the other hand, the lowest commmon denominator audio for Linux
 *  is probably S16LE 44100 Hz Stereo.
 */



/** Samples per seconds of the audio engine */
#define AUDIO_SAMPLE_RATE_IN_HZ (44100u)

/** The size of the audio buffer.  Consequently the maximum note length. */
#define AUDIO_BUFFER_DURATION_IN_MILLISECONDS (5000u)

/** Number of samples in the audio buffer */
#define AUDIO_BUFFER_SIZE \
  ((AUDIO_BUFFER_DURATION_IN_MILLISECONDS * AUDIO_SAMPLE_RATE_IN_HZ) / 1000u)

/** Number of independent channels */
#define AUDIO_CHANNEL_COUNT 16

/** Maximum amplitude of PCM waveform per channel */
#define AUDIO_CHANNEL_AMPLITUDE_MAX 32767
#define AUDIO_CHANNEL_AMPLITUDE_MAX_F ((double)(AUDIO_CHANNEL_AMPLITUDE_MAX))
/** Minimum amplitude of a PCM waveform per channel */
#define AUDIO_CHANNEL_AMPLITUDE_MIN (-32767)

/** Initializes the audio model. It zeros out the buffers and sets the
 *   'last updated' time to the current loop time.
 */
void audio_model_initialize (void);

/** Adds a tone or noise waveform to an audio channel.
    @param channel
        pre-mixed audio channel, 0 to AUDIO_CHANNEL_COUNT-1
    @param start_time
        seconds since program start when the sound is supposed to play, or 0.0 for 'now'
    @param D_attack
        duration of the attack of the ADSR envelope
    @param D_decay
        duration of the decay of the ADSR envelope
    @param D_sustain
        duration of the sustain of the ADSR envelope
    @param D_release
        duration of the release of the ADSR envelope
    @param F_initial
        initial frequency in Hz of the tone in Hz
    @param F_attack
        frequency of the tone after D_attack in Hz
    @param F_sustain
        frequency of the tone after D_decay in Hz
    @param F_release
        frequency of the tone after D_release in Hz
    @param A_attack
        amplitude after D_attack, 0.0 to 1.0
    @param A_sustain
        amplitude after D_decay, 0.0 to 1.0
    @param duty
        relative width of the upper half-period of an oscillation, 0.0 to 1.0
    @param noise
        0 => tone, 1 => noise
    @param waveform
        0 => square, 1 => sine
*/
void audio_model_add_tone(int channel, double start_time,
                          double D_attack, double D_decay, double D_sustain,
                          double D_release, double F_initial, double F_attack,
                          double F_sustain, double F_release, double A_attack,
                          double A_sustain, double duty, int noise,
                          int waveform);

/**  Adds a resource waveform to an audio channel
    @param channel
        pre-mixed audio channel, 0 to AUDIO_CHANNEL_COUNT-1
    @param start_time
        seconds since program start when the sound is supposed to play, or
        0.0 for 'now'
    @param ampliltude
        from 0.0 to 1.0, adjust the amplitude of the wave
    @param resource
        the index of a wave in the wave store
*/
void audio_model_add_wave (int channel, double start_time, double amplitude,
					 int index);

/**  Returns a pointer to the mixed waveform
 */
int16_t *audio_model_get_mixed();

/** De-queues samples from the audio model
    @param n
        Number of samples to be removed
*/
void audio_model_dequeue(unsigned n);

void am_init_guile_procedures (void);
#endif
