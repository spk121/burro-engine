/* tmx.h
 *
 * If I use the TMX C library, it needs to do two things.
 * - Fill out the BG and OBJ layers for a given map.
 * - Extract all the objects and re-express them as scheme objects
 *
 * TMX is much more free form than Burro, so if TMX requests something
 * that Burro can't do, it needs to handle it and throw a warning.
 */

