#ifndef BURRO_XISO9660_H
#define BURRO_XISO9660_h

#include <cdio/iso9660.h>

/** Close previously opened ISO 9660 image and free resources
 *   associated with the image. Call this when done using using an ISO
 *   9660 image.
 */
void xiso9660_close (iso9660_t * p_iso);


/** Open an ISO 9660 image for reading.
 *   @return NULL is returned on error.
 */
iso9660_t *xiso9660_open (const char *psz_path);

/** Fill a memory location with data from a file on an ISO9660 image.
 *   If the buffer is not large enough, fill as much as possible.
 *   @param p_iso
 *       The ISO9660 image handle
 *   @param name
 *       The filename of a resource in an ISO9660 image
 *   @param buf
 *       The location to which the resource will be copied
 *   @param maxlen
 *       The maximum size (in bytes) that can be placed in BUF
 *   @param len
 *       The actual number of bytes placed in BUF
 *   @return TRUE if the resource was found
 */
bool xiso9660_fill_data (iso9660_t *p_iso, const char *name,
                         uint8_t *buf, int maxlen, int *len);
#endif

