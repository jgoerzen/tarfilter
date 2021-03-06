/*
Copyright (C) 2008 John Goerzen <jgoerzen@complete.org>

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
*/

#define _FILE_OFFSET_BITS 64
#include <stdio.h>
#include <archive.h>
#include <archive_entry.h>
#include <inttypes.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>

#define TARENC_BUFSIZE 512

struct mydata {
  const char *name;
  int fd;
  off_t offset;
  off_t cmpoffset;
  char buff[1024];
  FILE *copytofile;
};

struct mypipes {
  int toencoder_r;
  int toencoder_w;
  int countreport_r;
  int countreport_w;
  int encoderpid;
  int counterpid;
};

void convert_archive(char *encoder, FILE *offsetf);
int myopenstdin(struct archive *a, void *client_data);
int myclose(struct archive *a, void *client_data);
ssize_t myread(struct archive *a, void *client_data, const void **buff);
void checkblock(off_t offset);
void errorexit(char *msg);
void archiveerrorexit(struct archive *a, char *msg);
int checkerror(char *msg, int code);
void setupsegment(struct mydata *mydata, struct mypipes *pipes, char *encoder);
void initpipes(struct mypipes *pipes);
off_t closepipes(struct mypipes *pipes);
void forkencoder(struct mypipes *pipes, char *encoder);


int main(int argc, char **argv) {
  FILE *offsetf;
  if (argc != 3) {
    printf("Usage:\n");
    printf("tarf-encoder segmentcmd offsetfile\n");
    return(1);
  }
  offsetf = fopen(argv[2], "wt");
  if (offsetf == NULL) {
    errorexit(argv[1]);
  }

  fprintf(offsetf, "uncoff\tcmpoff\tuncsize\tcmpsize\tfilename\n");

  convert_archive(argv[1], offsetf);
  return(0);
}

/* Unconditionally print the given message and exit. */
void errorexit(char *msg) {
  perror(msg);
  exit(5);
}

void archiveerrorexit(struct archive *a, char *msg) {
  fprintf(stderr, "%s: %s\n", msg, archive_error_string(a));
  exit(15);
}
  
/* If code is < 0, print the given message and exit. */
int checkerror(char *msg, int code) {
  if (code < 0) {
    errorexit(msg);
  }
  return(code);
}
 
/* Check to make sure that a given offset is a multiple of the blocksize. */
void checkblock(off_t offset) {
  if (offset % TARENC_BUFSIZE != 0) {
    fprintf(stderr, "\nWARNING: offset %" PRId64 " is not a multiple of %d!\n", offset, TARENC_BUFSIZE);
  }
}

/* Main conversion function */
void convert_archive(char *encoder, FILE *offsetf)
{
  struct mydata *mydata;
  struct mypipes *mypipes;
  
  struct archive *a;
  struct archive_entry *entry;
  off_t startingoffset, cmpsize;
  const void *tmpbuf;
  const char *filename;
  int readresult;

  /* libarchive seems to read one block at open; special case this */
  int offsetcorrection = TARENC_BUFSIZE;

  mydata = malloc(sizeof(struct mydata));
  mydata->copytofile = NULL;

  mypipes = malloc(sizeof(struct mypipes));
  
  setupsegment(mydata, mypipes, encoder);

  a = archive_read_new();
  mydata->name = "(stdin)";

  archive_read_support_compression_none(a);
  archive_read_support_format_tar(a);
  archive_read_open(a, mydata, myopenstdin, myread, myclose);

  while (1) {
    fprintf(offsetf, "%" PRId64 "\t%" PRId64 "\t", 
            mydata->offset - offsetcorrection,
            mydata->cmpoffset);

    // printf("block %" PRId64 ": ", 
    //        (mydata->offset - offsetcorrection) / TARENC_BUFSIZE);

    checkblock(mydata->offset);
    startingoffset = mydata->offset - offsetcorrection;

    readresult = archive_read_next_header(a, &entry);

    if (readresult == ARCHIVE_EOF) {
      filename = "** Block of NULs **";
      while (myread(a, mydata, &tmpbuf)) {};
      /*
      printf("  + hdr blocks: %" PRId64 "\n", 
             (mydata->offset - offsetcorrection - startingoffset / TARENC_BUFSIZE));
             } */
      break;
    }

    if (readresult != ARCHIVE_OK) {
      archiveerrorexit(a, "Error reading header");
    }
    filename = archive_entry_pathname(entry);
    /*
    printf("%s\n",archive_entry_pathname(entry));
    printf("  + hdr blocks: %" PRId64, 
           (mydata->offset + offsetcorrection - startingoffset) / TARENC_BUFSIZE);
    */
    offsetcorrection = 0;
    checkblock(mydata->offset);
    // startingoffset = mydata->offset;

    if (archive_read_data_skip(a) != ARCHIVE_OK) {
      archiveerrorexit(a, "Error reading data");
    }
    //printf(", + data blocks: %" PRId64 "\n", (mydata->offset - startingoffset) / TARENC_BUFSIZE);
    checkblock(mydata->offset);

    fclose(mydata->copytofile);
    cmpsize = closepipes(mypipes);

    fprintf(offsetf, "%" PRId64 "\t%" PRId64 "\t%s\n",
            mydata->offset - startingoffset,
            cmpsize, filename);
    mydata->cmpoffset += cmpsize;

    setupsegment(mydata, mypipes, encoder);
  }
  
  fclose(mydata->copytofile);
  cmpsize = closepipes(mypipes);
  fprintf(offsetf, "%" PRId64 "\t%" PRId64 "\t%s\n",
          mydata->offset - startingoffset, cmpsize, filename);
  archive_read_finish(a);
  free(mydata);
  free(mypipes);
}

/* Set up pipes, fork the encoder, and set up writing for a new segment. */
void setupsegment(struct mydata *mydata, struct mypipes *pipes, char *encoder) {
  initpipes(pipes);
  forkencoder(pipes, encoder);
  mydata->copytofile = fdopen(pipes->toencoder_w, "wb");
  if (mydata->copytofile == NULL) {
    errorexit("fdopen toencoder_w");
  }
}

/* Set up pipes. */
void initpipes(struct mypipes *pipes) {
  int filedes[2];
  
  checkerror("initpipes", pipe(filedes));
  pipes->toencoder_r = filedes[0];
  pipes->toencoder_w = filedes[1];

  checkerror("initpipes", pipe(filedes));
  pipes->countreport_r = filedes[0];
  pipes->countreport_w = filedes[1];
}

/* Fork encoder and counter */
void forkencoder(struct mypipes *pipes, char *encoder) {
  pid_t counterpid, encoderpid;
  int newfiledes[2];
  off_t bytecount = 0;
  char buff[TARENC_BUFSIZE * 4];
  ssize_t readcount = 0;
  int writecount = 0;
  FILE *writereport;

  // Set up pipe for communication between encoder and counter
  checkerror("forkencoder pipe", pipe(newfiledes));
    
  fflush(NULL);                 /* Make sure all buffers are flushed now */

  counterpid = checkerror("fork counter", fork());
  if (counterpid > 0) {           /* Main parent */
    pipes->counterpid = counterpid;

    encoderpid = checkerror("fork encoder", fork());
    if (encoderpid > 0) {       /* Main parent continued */
      pipes->encoderpid = encoderpid;

      close(pipes->toencoder_r);
      close(pipes->countreport_w);
      close(newfiledes[0]);
      close(newfiledes[1]);
    } else {                    /* Encoder */
      close(pipes->toencoder_w);
      close(pipes->countreport_w);
      close(pipes->countreport_r);
      close(newfiledes[0]);

      dup2(newfiledes[1], 1);
      close(newfiledes[1]);

      dup2(pipes->toencoder_r, 0);
      close(pipes->toencoder_r);

      checkerror("exec encoder", execl("/bin/sh", "/bin/sh", "-c", encoder, NULL));
    }
  } else {                      /* Counter */
    close(pipes->toencoder_r);
    close(pipes->toencoder_w);
    close(pipes->countreport_r);
    close(newfiledes[1]);
    
    while (1) {               /* there is data to read */
      readcount = checkerror("counter read",
                             read(newfiledes[0], buff, TARENC_BUFSIZE));
      if (readcount == 0) {
        break;
      }
      bytecount += (off_t) readcount;
      writecount = 0;
      while (readcount > writecount) {
        writecount += checkerror("counter write",
                                 (int) write(1, buff + writecount, readcount - writecount));
      }
    }
    writereport = fdopen(pipes->countreport_w, "wt");
    fprintf(writereport, "%" PRId64 "\n", bytecount);
    fclose(writereport);
    exit(0);
  } /* Counter */
}

/* Close pipes and wait for encoder and counter */
off_t closepipes(struct mypipes *pipes) {
  FILE *reader;
  int scanfresult;
  off_t retval;
  
  reader = fdopen(pipes->countreport_r, "rt");
  if (reader == NULL) {
    errorexit("fdopen pipes->countreport_r");
  }
  
  scanfresult = fscanf(reader, "%" SCNd64, &retval);
  if (scanfresult == EOF) {
    errorexit("scanf reader");
  } else {
    if (scanfresult != 1) {
      fprintf(stderr, "Got unexpected scanf result: %d\n", scanfresult);
      exit(6);
    }
  }

  fclose(reader);
  // handled by fclose above: close(pipes->toencoder_w);
  // handled by fclose(reader): close(pipes->countreport_r);

  waitpid(pipes->encoderpid, NULL, 0);
  waitpid(pipes->counterpid, NULL, 0);
  return retval;
}
  
/* Read callback */
ssize_t myread(struct archive *a, void *client_data, const void **buff)
{
  struct mydata *mydata = client_data;
  size_t bytesread = 0, thisread;

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
    bytesread += (size_t) thisread;
  }
               
  mydata->offset += (off_t) bytesread;
  if (mydata->copytofile != NULL) {
    if (fwrite(mydata->buff, 1, (size_t) bytesread, mydata->copytofile) != bytesread) {
      errorexit("writing to copytofile");
    }
  }

  return(bytesread);
}

int myopenstdin(struct archive *a, void *client_data)
{
  struct mydata *mydata = client_data;

  mydata->fd = 0;
  mydata->offset = 0;
  mydata->cmpoffset = 0;
  return(ARCHIVE_OK);
}

int myclose(struct archive *a, void *client_data)
{
  struct mydata *mydata = client_data;

  if (mydata->fd > 0)
    close(mydata->fd);
  return (ARCHIVE_OK);
}
