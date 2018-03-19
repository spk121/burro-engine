#include <glib.h>
#include <gio/gio.h>
#include <string.h>
#include <stdlib.h>
#include "targa.h"
//#include "targa_extra.h"
#include "palettize.h"
//#include "colors.h"

gchar *output;
gchar *tiles;
gchar *bitmap;
gchar *info;
gchar *palette;
gchar *reference;
gchar **input;
gchar *output;
static targa_image_t *targa;
static gboolean verbose = FALSE;
static gboolean map = FALSE;
static gboolean bmp8 = FALSE;
static gboolean bmp16 = FALSE;

static GOptionEntry entries[] = 
  {
    { "verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "verbose", NULL },
    { "map", 'm', 0, G_OPTION_ARG_NONE, &map, "convert a map file to a 24-bit true color image", NULL },
    { "bmp8", 'b', 0, G_OPTION_ARG_NONE, &bmp8, "convert an image to a 15-bit indexed image", NULL },
    { "bmp16", 't', 0, G_OPTION_ARG_NONE, &bmp16, "convert a 24 or 32-bit true color image to a 16-bit high-color image", NULL },
    { "output", 'o', 0, G_OPTION_ARG_FILENAME, &output, "output file name", "filename"},
    { G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &input, "input", "filename"},
    { NULL }
  };


static void convert (targa_image_t *targa, gboolean bmp8, gboolean bmp16, gboolean bmp32);


static void
print_targa_info (const gchar *filename, const targa_image_t *t)
{
  g_print("%s:\n", filename);
  g_print("\t");
  if (t->header.image_type == TARGA_IMAGE_TYPE_NO_IMAGE_DATA)
    g_print ("0x0 pixel ");
  else 
    g_print ("%ux%u pixel ", t->header.image_width, t->header.image_height);
  if (t->header.image_type == TARGA_IMAGE_TYPE_NO_IMAGE_DATA)
    g_print ("imageless ");
  else if (t->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED
	   || t->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED)
    g_print ("%ubpp indexed color ", t->header.pixel_depth);
  else if (t->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE
	   || t->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)
    g_print ("%ubpp grayscale ", t->header.pixel_depth);
  else if (t->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR
	   || TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR)
    {
      g_print ("%ubpp true color ", t->header.pixel_depth);
    }
  g_print ("image ");
  if (t->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED
      || t->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED)
    g_print ("with a %u entry %ubpp color map", t->header.color_map_length, t->header.color_map_entry_size);
  g_print ("\n");
}	

int 
main (int argc, char *argv[])
{
  GOptionContext *context;
  GError *error = NULL;
  gboolean ret;
  guint input_files_count;
	
  g_type_init();

  context = g_option_context_new ("- converts the bit depth of of Targa image files");
  g_option_context_add_main_entries (context, entries, NULL);
  ret = g_option_context_parse (context, &argc, &argv, &error);
  if (ret == FALSE)
    {
      g_print ("bad command line");
      return (2);
    }
  input_files_count = 0;
  if (input)
    input_files_count = g_strv_length (input);
  if (input_files_count == 0)
    {
      g_print ("Nothing to do\n");
      return (1);
    }

  GFile *file;
  GFileInputStream *fp;
  GError *gerror = NULL;
  targa_error_t err = 0;

  file = g_file_new_for_commandline_arg (input[0]);
  fp = g_file_read (file, NULL, &gerror);
  if (fp == NULL)
    {
      g_print ("ERROR: %s could not be opened for reading: %s", input[0], gerror->message);
      exit (1);
    }

  targa = g_new0 (targa_image_t, 1);

  if (map)
    {
      map_parse_stream (G_INPUT_STREAM(fp), targa);
    }
  else
    err = targa_parse_stream (G_INPUT_STREAM(fp), targa);
  if (err)
    {
      g_print ("%s is an invalid targa file\n", input[0]);
      exit (0);
    }

  print_targa_info (input[0], targa);

  if (!output)
    {
      g_print ("No output file specified: nothing to do\n");
      exit (0);
    }

  if (bmp8 || bmp16)
    convert (targa, bmp8, bmp16, 0);

    {
      gchar *name;
      GFile *file;
      GFileOutputStream *fp;

      /* Write the targa in its current state */
      name = g_strdup_printf ("%s", output);
      file = g_file_new_for_commandline_arg (name);
      g_free (name);
      fp = g_file_replace (file, NULL, FALSE, 0, NULL, NULL);
      targa_write (G_OUTPUT_STREAM(fp), targa);
      g_output_stream_close (G_OUTPUT_STREAM(fp), NULL, NULL);

      /* Write the targa as 32-bit, so I can compare */
      convert (targa, 0, 0, 1);
      name = g_strdup_printf ("%s_ref.tga", output);
      file = g_file_new_for_commandline_arg (name);
      g_free (name);
      fp = g_file_replace (file, NULL, FALSE, 0, NULL, NULL);
      targa_write (G_OUTPUT_STREAM(fp), targa);
      g_output_stream_close (G_OUTPUT_STREAM(fp), NULL, NULL);
    }
  return 0;
}

static void
convert (targa_image_t *targa, gboolean bmp8, gboolean bmp16, gboolean bmp32)
{
  if (bmp8)
    {
      targa_convert_to_indexed (targa);
      targa_convert_to_color (targa, COLOR_x1r5g5b5);
    }
  if (bmp16)
    {
      // targa_transparent_to_magenta
      targa_convert_to_nonindexed (targa);
      targa_convert_to_color (targa, COLOR_x1r5g5b5);
    }
  if (bmp32)
    {
      targa_convert_to_nonindexed (targa);
      targa_convert_to_color (targa, COLOR_a8r8g8b8);
    }
}

