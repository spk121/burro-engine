/*-----------------------------------------------------------------------------
        audio_model.h
        Copyright (C) 2014
                Michael L. Gran (spk121)

        GPL3+
-----------------------------------------------------------------------------*/
/** \file audio_model.h
     audio defines and functionality

    This is a simple audio stream model.

    It has several parts.

    NOTE: the frequency, amplitude and duration of a sound created by
    a theoretical instrument.  This is a 4-element
    - an INSTRUMENT ID, from 1 to 128
    - frequency in Hz
    - duration in seconds
    - amplitude, a positive number, where 0.0 is silent and 1.0 is
      maximum volume

    INSTRUMENT: These are the data that represents how an theoretical
    instrument (string, piano, brass, reed, etc) converts a NOTE into
    a WAVE waveform.  This audio engine has a rather simple instrument
    engine.  It is an Attack, Decay, Sustain, Release engine.
    - Duration of attack, decay, and release, in seconds
    - Frequency adjustment of initial, attack, sustain, and release:
      a ratio, where 1.0 is the frequency of the given NOTE
    - Amplitude adjustments of attack and sustain. A ratio, where
      1.0 is the amplitude of the given NOTE.
    - Amplitude of the 1st through 6th harmonic.  A ratio, where
      1.0 is the amplitude relative to the fundamental frequency.
    - A noise flag
    - A duty cycle, from 0.1 to 0.9, with 0.5 meaning that the
      period of a sinusoid is made from two equal half-periods.
      0.1 meaning that the 1st half period of 10% of the period length
      whilst the 2nd half period is 90% of the period length, etc.

    INSTRUMENT MAP: a hash table that associates a INSTRUMENT ID from 1 to 128
    with a INSTRUMENT.

    WAVE: a signed raw 16-bit monophonic waveform. In Guile these are
    bytevectors that hold signed 16-bit data in native endianness.
    WAVEs can be loaded from a file or generated with a NOTE+INSTRUMENT.

    SOUNDS: These are WAVEs with the addition property of a start
    time.  The start time property which says they should be played
    either as soon as possible, or at some specific time in the
    future.  For SOUNDs with a start time, that start time is
    expressed in seconds relative to the PULSEAUDIO CLOCK.

    SOUND LIST: This is a list of all the sounds that have been
    enqueued for playback.

    At regular intervals, the pulseaudio engine will request a about a
    millisecond's worth of audio to be played back, at which time the
    audio model will generate that audio from the SOUNDs in the SOUND
    LIST.  When a SOUND has finished playing, it will automatically be
    removed from the SOUND LIST.

    PULSEAUDIO CLOCK: a monotically increasing (but not 100% stable)
    time in seconds, reported by the Pulseaudio subsystem.  Not to be
    confused with the LOOP CLOCK.

*/

#ifndef BURRO_AUDIO_H
#define BURRO_AUDIO_H

#include <stdint.h>
#include <stdbool.h>
#include <libguile.h>

/** The Nintendo DS had 16 channels.  Audio was resampled down
 *  to 10-bit 32kHz Stereo before it was sent to the hardware
 *  for playback.
 *
 *  On the other hand, the lowest commmon denominator audio for Linux
 *  is probably S16LE 44100 Hz Stereo.
 */

/** Samples per seconds of the audio engine */
#define AUDIO_SAMPLE_RATE_IN_HZ (44100u)

/** The size of the audio buffer.  Consequently the maximum duration
    of a single read by Pulseaudio. */
#define AUDIO_BUFFER_DURATION_IN_MILLISECONDS (5000u)

/** Number of samples in the audio buffer. */
#define AUDIO_BUFFER_SIZE \
  ((AUDIO_BUFFER_DURATION_IN_MILLISECONDS * AUDIO_SAMPLE_RATE_IN_HZ) / 1000u)

/** Maximum amplitude of PCM waveform per channel */
#define AUDIO_CHANNEL_AMPLITUDE_MAX 32767
#define AUDIO_CHANNEL_AMPLITUDE_MAX_F ((double)(AUDIO_CHANNEL_AMPLITUDE_MAX))
/** Minimum amplitude of a PCM waveform per channel */
#define AUDIO_CHANNEL_AMPLITUDE_MIN (-32767)

/** Number of harmonic frequencies in an instrument, including the fundamental. */
#define HARMONICS_NUM (7)

/** A C structure that represents an theoretical instrument, which is
    a conversion between a note and a waveform. */
struct _instrument_t
{
    double D_attack; ///< Duration of the attack of the ADSR envelope,
                     ///< in seconds
    double D_decay;  ///< Duration of the decay of the ADSR envelope,
                     ///< in seconds
    double D_release;  ///< Duration of the release of the ADSR
                       ///< envelope, in seconds
    double F_initial;  ///< Initial frequency adjustment of the
                       ///< tone. A ratio. 1.0 for nominal
    double F_attack;   ///< Frequency adjustment after D_attack. A
                       ///< ratio. 1.0 for nominal
    double F_sustain;  ///< Frequency adjustment after D_decay. A
                       ///< ratio. 1.0 for nominal
    double F_release;  ///< Frequency adjustment after D_release. A
                       ///< ratio. 1.0 for nominal
    double A_attack;   ///< Amplitude adjustment after D_attack. A
                       ///< ratio. 0.0 to 1.0.
    double A_sustain;  ///< Amplitude adjustment after D_decay. A
                       ///< ratio. 0.0 to 1.0.
    bool noise;        ///< When true, randomize amplitude to make it
                       ///< a chiptune noise
    double duty;       ///< Relative width of the upper half-period of
                       ///< an oscillation. 0.0 to 1.0. 0.5 for nominal.
    double A_harmonics[HARMONICS_NUM];  ///< Relative amplitude of the
                                        ///< harmonics to the
                                        ///< fundamental.  A
                                        ///< ratio. 0.0 to 2.0. 1.0 for nominal
};

/** A typedef for _instrument_t. */
typedef struct _instrument_t instrument_t;

/** A Guile SCM that holds the definition of the Guile instrument
    foreign object type. */
extern SCM G_instrument_tag;

/** In Guile, this is <tt>(list->instrument x)</tt>.  A Guile
    procedure to convert a list of values into a Guile instrument
    foreign object val.

    @param x
        a list of the 17 elements in an _instrument_t struct
*/
SCM G_list_to_instrument (SCM x);

/** In Guile this is <tt>(instrument-generate-wave instrument
    frequency duration amplitude)</tt>.  Renders a note into a Guile
    bytevector containing a native-endian, signed, 16-bit waveform
    sampled at AUDIO_SAMPLE_RATE_IN_HZ.  The rendering is according to
    an instrument model.

    @param instrument
        An instrument foreign object val
    @param frequency
        A real number. The fundamental frequency of the note, in Hz.
    @param duration
        A real number.  The duration of the note in seconds.
    @param amplitude
        A real number. The maximum amplitude of the waveform, ideally between 0.1 and 1.0.
*/
SCM G_instrument_generate_wave (SCM instrument, SCM frequency, SCM duration, SCM amplitude);

/** In Guile, this is <tt>%%sound-playlist</tt>. A Guile variable that
    contains the list of sounds that are to be played.  Each sound is
    a Guile pair, where CAR is the time at which the sound should be
    played and CDR is the bytevector containing a signed, 16-bit,
    native-endian waveform sampled at AUDIO_SAMPLE_RATE_IN_HZ. */
extern SCM G_sound_playlist_var;

/** In Guile, this is <tt>(play-wave wave #:optional start-time)</tt>.
    Add a waveform to the G_sound_playlist_var, so that it will
    be played in the future.
    @param wave
        A bytevector. A native-endian, signed, 16-bit waveform sampled at
        AUDIO_SAMPLE_RATE_IN_HZ
    @param start_time
        A time, in seconds, according to the Pulseaudio clock, when
        the wave should start playing.  If the start_time is
        SCM_UNSPECIFIED, it will play as soon as possible.
*/
SCM G_play_wave (SCM wave, SCM start_time);

/** In Guile, this is <tt>(audio-time)</tt>.  Returns an estimate
    of the current Pulseaudio timer, in seconds. */
SCM G_audio_time ();

/** In Guile, this is <tt>(audio-last-update-time)</tt>.  Returns the
    time, reported by the Pulseaudio timer, when the last audio update
    was requested by the Pulseaudio subsystem. */
SCM G_audio_last_update_time ();

/**  Used by pulseaudio, this function mixes down all the sounds
     in G_sound_playlist_var beginning at a start time and continuing
     for a given number of samples.  The return value should not
     be freed by the caller.
     @param time_start
        A time, in seconds, of the pulseaudio clock.
     @param n
        Number of samples requested at the sample rate
        AUDIO_SAMPLE_RATE_IN_HZ.
 */
const int16_t *audio_model_get_wave(double time_start, size_t n);


void am_init_guile_procedures (void);
#endif
