#include <stdio.h>
#include <inttypes.h>
#include <stddef.h>

void write_vector(FILE *fd, const char *name, const uint8_t *buf, size_t size);
void seek_vector(FILE *fd, const char *name);
size_t read_vector(FILE *fd, uint8_t *vector);
