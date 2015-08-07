/** @file  wave.h
    @brief Storage for short audio samples
*/

#ifndef BURRO_WAVE_H
#define BURRO_WAVE_H

/** The max number of long samples */
#define LONG_WAVE_COUNT_NUM (5)

/** The maximum length, in samples, of a long sample */
#define LONG_WAVE_I16_SIZE (44100 * 60)

/** The max number of short samples */
#define SHORT_WAVE_COUNT   (100)

/** The maximum length, in samples, of a short sample */
#define SHORT_WAVE_I16_SIZE (44100)

extern int16_t wave_store[LONG_WAVE_COUNT * LONG_WAVE_I16_SIZE
                          + SHORT_WAVE_COUNT * SHORT_WAVE_I16_SIZE] __attribute__ (( aligned (16)));

////////////////////////////////////////////////////////////////

/** Return the size, in 16-bit words, of a short wave store.
 */
int wave_get_i16_size (int index);

/** Return a pointer to the beginning of a short wave
 */
int16_t *wave_get_i16_ptr (int index);

/** Zero-fill the contents of a short wave
 */
void wave_zero_sample (int index);

/** Fill a short wave from a file or resource
 */
bool wave_add_from_file (int index, char *filename);

/** Register wave procedures with the script engine */
void wave_init_guile_procedures (void);

#endif
