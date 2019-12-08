#include "image.h"
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <limits.h>

typedef struct image {
  char *data;
  off_t size;
  int width;
  int height;
} image;


void *image_alloc (size_t sz) {
  return malloc(sz);
}

off_t fsize(const char *filename) {
  struct stat st;

  if (stat(filename, &st) == 0)
    return st.st_size;

  return -1;
}

image *read_image(char *filename, int width, int height) {
  off_t sz = fsize(filename);
  int fd = open(filename, O_RDONLY|O_SYMLINK);
  void *data = mmap(NULL, sz, PROT_READ, MAP_FILE | MAP_SHARED, fd, 0);
  image* img = malloc(sizeof(image));

  img->data = data;
  img->size = sz;
  img->width = width;
  img->height = height;

  return img;
}

int number_of_layers(image *img) {
  return img->size / (img->width * img->height);
}

int number_of(image *img, int layer, char digit) {
  int layer_sz = img->width * img->height;
  int offset = layer_sz * layer;
  int count = 0;

  for(int i = 0; i < layer_sz; i++) {
    if (img->data[i + offset] == digit) {
      count++;
    }
  }

  return count;
}

int layer_with_fewer_digits(image *img, char digit) {
  int num_layers = number_of_layers(img);
  int min_digits = INT_MAX;
  int min_layer = -1;

  for(int layer=0; layer < num_layers; layer++) {
    int numd = number_of(img, layer, digit);
    if(numd < min_digits) {
      min_layer = layer;
      min_digits = numd;
    }
  }

  return min_layer;
}

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
