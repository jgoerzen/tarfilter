#define _FILE_OFFSET_BITS 64
#include <stdio.h>
#include <archive.h>
#include <archive_entry.h>
#include <inttypes.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

#define TARENC_BUFSIZE 512

struct mydata {
  const char *name;
  int fd;
  off_t offset;
  char buff[1024];
};

void list_archive(void);
int myopenstdin(struct archive *a, void *client_data);
int myclose(struct archive *a, void *client_data);
ssize_t myread(struct archive *a, void *client_data, const void **buff);
void checkblock(off_t offset);

int main(void) {
  list_archive();
  return(0);
}
  
void checkblock(off_t offset) {
  if (offset % TARENC_BUFSIZE != 0) {
    printf("\nWARNING: offset %" PRId64 " is not a multiple of %d!\n", offset, TARENC_BUFSIZE);
  }
}

void
list_archive(void)
{
  struct mydata *mydata;
  struct archive *a;
  struct archive_entry *entry;
  off_t startingoffset;
  void *tmpbuf;

  /* libarchive seems to read one block at open; special case this */
  int offsetcorrection = TARENC_BUFSIZE;

  mydata = malloc(sizeof(struct mydata));
  a = archive_read_new();
  mydata->name = "(stdin)";
  archive_read_support_compression_none(a);
  archive_read_support_format_tar(a);
  archive_read_open(a, mydata, myopenstdin, myread, myclose);
  while (1) {
    printf("block %" PRId64 ": ", 
           (mydata->offset - offsetcorrection) / TARENC_BUFSIZE);
    checkblock(mydata->offset);
    startingoffset = mydata->offset;
    if (archive_read_next_header(a, &entry) != ARCHIVE_OK) {
      printf("** Block of NULs **\n");
      while (myread(a, mydata, &tmpbuf)) {};
      printf("  + hdr blocks: %" PRId64 "\n", 
             (mydata->offset - offsetcorrection - startingoffset / TARENC_BUFSIZE));
      break;
    }
    printf("%s\n",archive_entry_pathname(entry));
    printf("  + hdr blocks: %" PRId64, 
           (mydata->offset + offsetcorrection - startingoffset) / TARENC_BUFSIZE);
    offsetcorrection = 0;
    checkblock(mydata->offset);
    startingoffset = mydata->offset;
    archive_read_data_skip(a);
    printf(", + data blocks: %" PRId64 "\n", (mydata->offset - startingoffset) / TARENC_BUFSIZE);
    checkblock(mydata->offset);
  }
  archive_read_finish(a);
  free(mydata);
}

ssize_t
myread(struct archive *a, void *client_data, const void **buff)
{
  struct mydata *mydata = client_data;
  ssize_t bytesread = 0, thisread;

  /* Try really hard to get things in exactly TARENC_BUFSIZE bytes. */

  *buff = mydata->buff;

  while (bytesread < TARENC_BUFSIZE) {
    thisread = read(mydata->fd, mydata->buff + bytesread, 
                    TARENC_BUFSIZE - bytesread);
    if (thisread < 0) {
      return(thisread);
    }
    if (thisread == 0) {
      break;
    }
    bytesread += thisread;
  }
               
  mydata->offset += (off_t) bytesread;
  return(bytesread);
}

int
myopen(struct archive *a, void *client_data)
{
  struct mydata *mydata = client_data;

  mydata->fd = open(mydata->name, O_RDONLY);
  mydata->offset = 0;
  return (mydata->fd >= 0 ? ARCHIVE_OK : ARCHIVE_FATAL);
}

int
myopenstdin(struct archive *a, void *client_data)
{
  struct mydata *mydata = client_data;

  mydata->fd = 0;
  mydata->offset = 0;
  return(ARCHIVE_OK);
}

int
myclose(struct archive *a, void *client_data)
{
  struct mydata *mydata = client_data;

  if (mydata->fd > 0)
    close(mydata->fd);
  return (ARCHIVE_OK);
}
