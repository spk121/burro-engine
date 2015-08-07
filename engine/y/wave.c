#include "wave.h"

int16_t wave_store[LONG_WAVE_COUNT * LONG_WAVE_I16_SIZE
                          + SHORT_WAVE_COUNT * SHORT_WAVE_I16_SIZE] __attribute__ (( aligned (16)));
int wave_length[LONG_WAVE_COUNT + SHORT_WAVE_COUNT];

int wave_get_i16_size (int index)
{
    g_assert (index >= 0 && index < LONG_WAVE_COUNT + SHORT_WAVE_COUNT);
    return wave_length[i];
}

int16_t *wave_get_i16_ptr (int index)
{
    g_assert (index >= 0 && index < LONG_WAVE_COUNT + SHORT_WAVE_COUNT);
    if (index < LONG_WAVE_COUNT)
        return (index * LONG_WAVE_I16_SIZE);
    else
        return LONG_WAVE_COUNT * LONG_WAVE_I16_SIZE
            + (index - LONG_WAVE_COUNT) * SHORT_WAVE_I16_SIZE;
}

void wave_zero_sample (int index)
{
    g_assert (index >= 0 && index < LONG_WAVE_COUNT + SHORT_WAVE_COUNT);

    memset (wave_get_i16_ptr(index), 0, wave_get_i16_size (index));
}

void wave_load_from_file (int index, char *filename)
{
    int16_t *buf = wave_get_i16_ptr (index);
    int len = wave_get_i16_size (index);
    load_pcm_from_file (buf, &len, filename);
    wave_length[index] = len;
}

SCM_DEFINE (G_wave_load_from_file, "wave-load-from_file", 0, 0, 0, (SCM index, SCM filename), "\
Load I16LE PCM data from a file into a wave store location")
{
    wave_load_from_file (scm_to_int (index),
                         scm_to_locale_string (filename));
}

void wave_init_guile_procedures (void)
{
    #include "wave.x"
    scm_c_export ("wave-load-from-file",
                  "wave-bytevector",
                  "wave-get-max-length",
                  "wave-get-length",
                  "wave-set-length")
}
