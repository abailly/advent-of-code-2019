#include "image.h"
#include <stdio.h>

int main (int argc, char **argv) {
  char *fname = argv[1];
  int w = atoi(argv[2]);
  int h = atoi(argv[3]);

  image *img = read_image(fname, w, h);
  printf("image:  %lld (%d x %d)\n", img->size, img->width, img->height);

  int layer = layer_with_fewer_digits(img, '0');

  printf("layer %d : %d\n", layer, number_of(img, layer, '1') * number_of(img, layer, '2'));

  return 0;
}
