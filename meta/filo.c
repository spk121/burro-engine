#include <stdio.h>

int main(int argc, char ** argv)
{
  printf("argc %d\n", argc);
  for (int i = 0; i < argc; i ++)
    printf("argv[%d] %s\n", i, argv[i]);

  ssize_t ret;
  size_t N;
  char *line;

  FILE *fp = fopen(argv[1], "rt");
  do {
    line = NULL;
    N = 0;
    ret = getline (&line, &N, fp);
    printf("%zd %zu: %s\n", ret, N, line);
  } while (ret != -1);
  return 0;
}
