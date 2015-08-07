#include <unistd.h>
#include "xiso9660.h"

static int round_up(int numToRound, int multiple)
{
    if(multiple == 0)
        return numToRound;

    int remainder = numToRound % multiple;
    if (remainder == 0)
        return numToRound;
    return numToRound + multiple - remainder;
}


void xiso9660_close (iso9660_t *p_iso)
{
    iso9660_close (p_iso);
}

iso9660_t *xiso9660_open (const char *path)
{
    return iso9660_open (path);
}

bool
xiso9660_fill_data (iso9660_t *p_iso, const char *name, uint8_t *buf, int maxlen, int *len)
{
    iso9660_stat_t *stat = iso9660_ifs_stat (p_iso, name);

    if (stat == NULL)
    {
        g_critical ("Can't open ISO file member '%s'", name);
        return false;
    }

    size_t true_length = p_stat->size;
    size_t block_length = round_up(p_stat->size, ISO_BLOCKSIZE);
    char block[ISO_BLOCKSIZE];

    if (true_length > maxlen)
        true_length = maxlen;
    
    // Read the data from the buffer in 2K blocks
    for (size_t i = 0; i < block_length; i += ISO_BLOCKSIZE) {
        size_t block_len = ISO_BLOCKSIZE;
        iso9660_iso_seek_read (p_iso, block, p_stat->lsn + (i / ISO_BLOCKSIZE), 1);
        if ((i+1) * ISO_BLOCKSIZE > true_length)
            block_len = true_length - i * ISO_BLOCKSIZE;
        memcpy (buf + i * ISO_BLOCKSIZE, block, block_len);
    }
    *len = true_length;
    return true;
}
