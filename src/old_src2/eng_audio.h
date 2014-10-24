#ifndef ENG_AUDIO_H_INCLUDED
#define ENG_AUDIO_H_INCLUDED

void audio_update(void);
void initialize_audio(void);
_Bool get_channel_is_playing(int channel);
void set_channel_is_playing(int channel, _Bool flag);

#endif // ENG_AUDIO_H_INCLUDED
