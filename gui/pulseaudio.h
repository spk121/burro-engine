#ifndef BURRO_PULSEAUDIO_H
#define BURRO_PULSEAUDIO_H

#include <stdint.h>

/* Set up the audio backend. */
void pulse_initialize_audio(void);

/* Do audio actions.  Return number of samples sent. */
void pulse_update_audio(void);

/* Tear down the audio backend. */
void pulse_finalize_audio(void);

#endif
