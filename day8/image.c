#include "image.h"
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <limits.h>


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

char pixel_at(image *img, int x, int y, int z) {
  int layer_sz = img->width * img->height;

  return img->data[(z * layer_sz) + y * (img->width) + x];
}

void decode_image(image *img, char *decode) {
  int num_layers = number_of_layers(img);
  int w = img->width;
  int h = img->height;

  for(int i =0; i < w; i++) {
    for(int j =0; j < h; j++) {
      for(int k=0; k < num_layers; k++) {
        int px = pixel_at(img, i, j, k);
        if(px != '2') {
          decode[i + j * w] = px;
          break;
        }
      }
    }
  }
}


void display_image(image *img, char *decoded, int fd) {
  char output[img->height][img->width + 1];
  int w = img->width;
  int h = img->height;


  for(int j =0; j < h; j++) {
    int i;
    for(i =0; i < w; i++) {
      if(decoded[i + (j * w) ] == '1') {
        output[j][i] = ' ';
      } else {
        output[j][i] = '#';
      }
    }
    output[j][i] = '\n';
    write(fd, &output[j], w + 1);
  }
}
