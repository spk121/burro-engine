/*  pulseaudio.h

    Copyright (C) 2018   Michael L. Gran
    This file is part of Burro Engine

    Burro Engine is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Burro Engine is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Burro Engine.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef BURRO_PULSEAUDIO_H
#define BURRO_PULSEAUDIO_H

#include <stdint.h>
#include "audio_model.h"

#define MICROSECONDS_PER_MILLISECOND (1000)

#define AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS (100)

/* Set up the audio backend. */
void pulse_initialize_audio_step_1(void);
void pulse_initialize_audio_step_2(pa_context *c);

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

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
