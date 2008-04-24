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
  int encoder_pid;
  int counter_pid;
}

void convert_archive(char *encoder, FILE *offsetf);
int myopenstdin(struct archive *a, void *client_data);
int myclose(struct archive *a, void *client_data);
ssize_t myread(struct archive *a, void *client_data, const void **buff);
void checkblock(off_t offset);
void errorexit(char *msg);
int checkerror(int code, char *msg);
void initpipes(struct mypipes *pipes);
off_t closepipes(struct mypipes *pipes);
void forkencoder(struct mypipes *pipes, char *encoder);

int main(int argc, char *argv) {
  FILE *offsetf;
  if (argc != 3) {
    printf("Usage:\n");
    printf("tarenc-segmenter segmentcmd offsetfile\n");
    return(1);
  }
  offsetf = fopen(argv[2], "wt");
  if (offsetf == NULL) {
    errorexit(argv[1]);
  }

  fprintf(offsetf, "uncoffset\tcmpoffset\tuncsize\tcmpsize\tfilename\n");

  convert_archive(argv[1], offsetf);
  return(0);
}

void errorexit(char *msg) {
  perror(msg);
  exit(5);
}
  
int checkerror(int code, char *msg) {
  if (code < 0) {
    errorexit(msg);
  }
  return(code);
}
 
void checkblock(off_t offset) {
  if (offset % TARENC_BUFSIZE != 0) {
    printf("\nWARNING: offset %" PRId64 " is not a multiple of %d!\n", offset, TARENC_BUFSIZE);
  }
}

void convert_archive(char *encoder, FILE *offsetf)
{
  struct mydata *mydata;
  struct mypipes *mypipes;
  
  struct archive *a;
  struct archive_entry *entry;
  off_t startingoffset, cmpsize;
  const void *tmpbuf;
  pid_t encoderpid, counterpid;
  char *filename;

  /* libarchive seems to read one block at open; special case this */
  int offsetcorrection = TARENC_BUFSIZE;

  mydata = malloc(sizeof(struct mydata));
  mydata->copytofile = NULL;

  mypipes = malloc(sizeof(struct mypipes));

  initpipes(mypipes);
  forkencoder(mypipes, encoder);
  mydata->copytofile = fdopen(mypipes->toencoder_w, "wb");
  if (mydata->copytofile == NULL) {
    errorexit("fdopen toencoder_w");
  }
  
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
    startingcmpoffset = mydata->cmpoffset;

    if (archive_read_next_header(a, &entry) != ARCHIVE_OK) {
      filename = "** Block of NULs **";
      while (myread(a, mydata, &tmpbuf)) {};
      /*
      printf("  + hdr blocks: %" PRId64 "\n", 
             (mydata->offset - offsetcorrection - startingoffset / TARENC_BUFSIZE));
             } */
      break;
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

    archive_read_data_skip(a);
    //printf(", + data blocks: %" PRId64 "\n", (mydata->offset - startingoffset) / TARENC_BUFSIZE);
    checkblock(mydata->offset);

    fclose(mydata->copytofile);
    cmpsize = closepipes(mypipes);

    fprintf(offsetf, "%" PRId64 "\t%" PRId64 "\t%s\n",
            mydata->offset - startingoffset,
            mydata->cmpoffset 
  }
  archive_read_finish(a);
  free(mydata);
}

void initpipes(struct mypipes *pipes) {
  int filedes[2];
  
  checkerror(pipe(filedes), "initpipes");
  pipes->toencoder_r = filedes[0];
  pipes->toencoder_w = filedes[1];

  checkerror(pipe(filedes), "initpipes");
  pipes->countreport_r = filedes[0];
  pipes->countreport_w = filedes[1];
}

void forkencoder(struct mypipes *pipes, char *encoder) {
  pid_t counterpid, encoderpid;
  int newfiledes[2];
  off_t bytecount = 0;
  char buff[TARENC_BUFSIZE * 4];
  ssize_t readcount = 0;
  int writecount = 0;
  FILE *writereport;

  // Set up pipe for communication between encoder and counter
  checkerror(pipe(newfiledes));
    
  counterpid = checkerror(fork());
  if (counterpid > 0) {           /* Main parent */
    pipes->counterpid = counterpid;

    encoderpid = checkerror(fork());
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

      checkerror(execl("/bin/sh", "-c", encoder));
    }
  } else {                      /* Counter */
    close(pipes->toencoder_r);
    close(pipes->toencoder_w);
    close(pipes->countreport_r);
    close(newfiledes[1]);
    
    while (1) {               /* there is data to read */
      readcount = checkerror(read(newfiledes[0], buff, TARENC_BUFSIZE));
      if (readcount == 0) {
        break;
      }
      bytecount += (off_t) readcount;
      writecount = 0;
      while (readcount > writecount) {
        writecount += checkerror((int) write(1, buff + writecount, readcount - writecount));
      }
    }
    writereport = fdopen(pipes->countreport_w, "wt");
    fprintf(writereport, "%" PRId64, bytecount);
    fclose(writereport);
    exit(0);
  } /* Counter */
}

off_t closepipes(struct mypipes *pipes) {
  char buff[1024];
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
    if (scanfresult != 0) {
      fprintf(stderr, "Got unexpected scanf result: %d\n", scanfresult);
      exit(6);
    }
  }

  fclose(reader);
  // handled by fclose above: close(pipes->toencoder_w);
  // handled by fclose(reader): close(pipes->countreport_r);

  waitpid(pipes->encoder_pid, NULL, 0);
  waitpid(pipes->counter_pid, NULL, 0);
  return retval;
}
  
ssize_t myread(struct archive *a, void *client_data, const void **buff)
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
  if (mydata->copytofile != NULL) {
    if (fwrite(mydata->buff, 1, bytesread, mydata->copytofile) != bytesread) {
      errorexit("writing to copytofile");
    }
  }

  return(bytesread);
}

int
myopenstdin(struct archive *a, void *client_data)
{
  struct mydata *mydata = client_data;

  mydata->fd = 0;
  mydata->offset = 0;
  mydata->cmpoffset = 0;
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
