#include "vectors.h"
#include <stdlib.h>
#include <string.h>

void write_vector(FILE *fd, const char *name, const uint8_t *buf, size_t size)
{
    fprintf(fd, "%s:\n", name);
    size_t idx = 0;
    while (idx < size) {
        fprintf(fd, "%03zu  ", idx);
        char bytes[49] = "                                                ";
        char print[17] = {0};
        char *b = bytes;
        char *p = print;
        for (int i = 0; i < 16; i++) {
            if (idx == size) {
                break;
            }
            sprintf(b, "%02x", buf[idx]);
            b[2] = ' '; // overwrites the '\0' written by sprintf()
            int is_printable = buf[idx] > 31 && buf[idx] < 127;
            p[0] = is_printable ? buf[idx] : '.';
            idx++;
            b += 3;
            p++;
        }
        fprintf(fd, "%s %s\n", bytes, print);
    }
}

// Does the current line matches name + ":\n" ?
// Note: long lines are ignored.
// The name must not exceed 126 characters.
static int match(FILE *fd, const char *name)
{
    char buf[128];
    if (fgets(buf, 128, fd) == NULL) {
        fprintf(stderr, "Vector not found\n");
        exit(1);
    }
    // ignore lines longer than 127 characters
    if (strlen(buf) == 127) {
        while (strlen(buf) == 127) {
            fgets(buf, 128, fd); // consume the rest of the line
        }
        return 0;
    }
    // If the line is short enough, compare it with the name
    const char *line = buf;
    while (*name == *line) {
        name++;
        line++;
    }
    if (name[0]  != '\0' || line[0] != ':' ) return 0; // line must end with ':'
    if (line [1] == '\n'                   ) return 1; // UNIX newline
    if (line [1] == '\r' && line[2] == '\n') return 1; // Windows newline
    return 0;                                          // not a newline
}

void seek_vector(FILE *fd, const char *name)
{
    while (!match(fd, name)) {}
}

static unsigned from_hex(char c)
{
    if (c >= '0' && c <= '9') { return c - '0';      }
    if (c >= 'a' && c <= 'f') { return c - 'a' + 10; }
    if (c >= 'A' && c <= 'F') { return c - 'A' + 10; }
    fprintf(stderr, "Wrong vector format: not a hexadecimal number\n");
    exit(1);
}

static size_t read_vector_line(FILE *fd, uint8_t *vector)
{
    char buf[128];
    fgets(buf, 128, fd);
    size_t nb_bytes = 0;
    char *s         = buf + 5;  // skip byte offset
    while (*s != ' ') {
        *vector = (from_hex(s[0]) << 4) + from_hex(s[1]);; // read byte
        s += 3;
        vector++;
        nb_bytes++;
    }
    return nb_bytes;
}

size_t read_vector(FILE *fd, uint8_t *vector)
{
    size_t size = 0;
    size_t line_size;
    do {
        line_size = read_vector_line(fd, vector);
        size     += line_size;
        vector   += line_size;
    } while (line_size == 16);
    return size;
}
