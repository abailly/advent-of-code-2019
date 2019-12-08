#ifndef IMAGE_H
#define IMAGE_H

#include <stdlib.h>
#include <sys/types.h>

typedef struct image {
  char *data;
  off_t size;
  int width;
  int height;
} image;

void *image_alloc (size_t sz);

image *read_image(char *filename, int width, int height);

int number_of_layers(image *img) ;

int number_of(image *img, int layer, char digit) ;

int layer_with_fewer_digits(image *img, char digit);

void decode_image(image *img, char *decode);

void display_image(image *img, char *decoded, int fd);

#endif // IMAGE_H
