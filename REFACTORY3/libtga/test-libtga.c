#include <libtga.h>
#include <stdio.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  FILE *fp;
  struct stat buf;
  uint8_t *mem;
  size_t len;
  int ret;
  char *fname;

  if (argc > 1)
    fname = strdup(argv[1]);
  else
    fname = "cga8c.tga";

  ret = lstat(fname, &buf);
  if (ret < 0) {
    printf("Can't find TGA test image %s\n", fname);
    return 1;
  }
  len = buf.st_size;
  mem = (uint8_t *) malloc(len);

  fp = fopen(fname, "rb");
  if (fp == NULL) {
    printf ("Can't open TGA test image %s\n", fname);
    return 1;
  }
  len = fread(mem, 1, len, fp);

  tga_ctx_t *libtga_ctx;
  ret = tga_new(&libtga_ctx);
  if (ret != TGA_OK)
    return ret;

  tga_image_t *img;
  ret = tga_image_new_from_memory(libtga_ctx, mem, len, &img);
  if (ret != TGA_OK)
    return ret;

  tga_image_unref(img);
  tga_unref (libtga_ctx);
  return 0;
}
