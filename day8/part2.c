#include "image.h"
#include <stdio.h>

int main (int argc, char **argv) {
  char *fname = argv[1];
  int w = atoi(argv[2]);
  int h = atoi(argv[3]);

  image *img = read_image(fname, w, h);
  printf("image:  %lld (%d x %d)\n", img->size, img->width, img->height);

  char msg[w][h];

  decode_image(img, (char *)msg);
  display_image(img,(char*)msg, 1);

  return 0;
}
