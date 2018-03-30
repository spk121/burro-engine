#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// For use with /usr/share/X11/rgb.txt

int main(int argc, char **argv)
{
  FILE *fp = fopen ("/usr/share/X11/rgb.txt", "rt");
  int r, g, b;
  size_t len = 100;
  char *line = malloc(len);
  size_t long_namelen = 0;
  size_t count = 0;

  // Pass 1, figure out the longest name.  
  while (!feof(fp))
    {
      fscanf(fp, "%d %d %d", &r, &g, &b);
      ssize_t n = getline(&line, &len, fp);
      int left, right;
      left = 0;
      while (line[left] == ' ' || line[left] == '\t')
	left++;
      right = n;
      while (line[right-1] == ' ' || line[right-1] == '\n')
	{
	  line[right-1] = '\0';
	  right --;
	}
      size_t namelen = strlen(line + left);
      if (namelen > long_namelen)
	long_namelen = namelen;

      count ++;
    }

  rewind(fp);

  printf("#include <stdint.h>\n");
  printf("#define COLOR_NAMES_COUNT %zu\n", count);
  printf("#define COLOR_NAMES_MAX_LEN %zu\n\n", long_namelen);
  
  printf("typedef struct color_name_tag {\n");
  printf("    char name[%zu];\n", long_namelen + 1);
  printf("    uint32_t val;\n");
  printf("} color_name_t;\n\n");

  printf("color_name_t color_names[%zu] = {\n", count);

  char *spacestr = malloc(long_namelen + 3);
  
  while (!feof(fp))
    {
      fscanf(fp, "%d %d %d", &r, &g, &b);
      ssize_t n = getline(&line, &len, fp);
      int left, right;
      left = 0;
      while (line[left] == ' ' || line[left] == '\t')
	left++;
      right = n;
      while (line[right-1] == ' ' || line[right-1] == '\n')
	{
	  line[right-1] = '\0';
	  right --;
	}
      memset (spacestr, 0, long_namelen + 1);
      for (size_t i = 0; i < long_namelen - strlen(line + left); i ++)
	     spacestr[i] = ' ';
      printf("    {\"%s\",%s0x%02x%02x%02x},\n",
	     line + left, spacestr, r, g, b);
    }
  printf("};\n");
  free (spacestr);
  free (line);

  fclose (fp);
  return 0;
}
