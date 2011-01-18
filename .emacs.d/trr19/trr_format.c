/* format.c -- a text formatter for TRR19
 * Last modified on Wed Mar 19 00:54:27 1997
 *               by SHUHEI, Yamaguchi <yamagus@kw.netlaputa.or.jp>
 * Copyright (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>
 */

/* This file is a part of TRR19, a type training software on
 * GNU Emacs.
 *
 * You can redistribute it and/or modify it under the terms of
 * the version 2 of GNU General Public License as published by the
 * Free Software Foundation.
 *
 * TRR19 is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#ifdef WIN32
#include <windows.h>
#define sleep(_sec) Sleep((DWORD) _sec * 1000)
#define DIR_SEP '\\'
#define UNIX_DIR_SEP '/'
/*
 * You'd need HAVE_STRING_H, HAVE_FCNTL_H, USE_TRRDIR, NO_GREP.
 */
#else /* WIN32 */
#define DIR_SEP '/'
#endif /* WIN32 */

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>

#if defined(HAVE_STRING_H)
#include <string.h>
#else
#include <strings.h>
#endif /* HAVE_STRING_H */  

#if defined(HAVE_FCNTL_H)
#include <fcntl.h>
#else
#if defined(HAVE_SYS_FCNTL_H)
#include <sys/fcntl.h>
#else
#include <sys/file.h>
#endif /* HAVE_SYS_FCNTL_H */
#endif /* HAVE_FCNTL_H */


main(int argc, char **argv){
#ifndef NO_GREP
  char textfile[256], formattedfile[256], lockfile[256], *tmpfname;
  char command[256], line[1024];
  FILE *fd, *tmpfd;
#else /* !NO_GREP */
  char textfile[256], formattedfile[256], lockfile[256];
  char line[1024];
  char *pl;
  FILE *fd, *txtfd;
#endif /* !NO_GREP */
#ifdef USE_TRRDIR
  char *env;
#ifdef WIN32
  char *pe;
#endif /* WIN32 */
#endif /* USE_TRRDIR */
  int i;

  /* ignore signals */
#ifdef SIGHUP
  signal(SIGHUP, SIG_IGN);
#endif /* SIGHUP */
  signal(SIGINT, SIG_IGN);
#ifdef SIGQUIT
  signal(SIGQUIT, SIG_IGN);
#endif /* SIGQUIT */
  signal(SIGTERM, SIG_IGN);

#ifndef USE_TRRDIR
  strcpy(textfile, TEXT_DIR);
#else  /* !USE_TRRDIR */
  if (!(env = getenv("TRRDIR"))) {
#ifdef TEXT_DIR
    strcpy(textfile, TEXT_DIR);
#else  /* TEXT_DIR */
    fprintf(stderr, "Can't get environment TRRDIR.\n");
    exit(1);
#endif /* TEXT_DIR */
  } else {
#ifdef WIN32
        for (pe = env; *pe; pe++) {
          if (*pe == UNIX_DIR_SEP) {
                *pe = DIR_SEP;
          }
        }
#endif /* WIN32 */
    sprintf(textfile, "%s%ctext%c", env, DIR_SEP, DIR_SEP);
  }
#endif /* !USE_TRRDIR */
  strcat(textfile, argv[1]);
  strcpy(formattedfile, textfile);
  strcat(formattedfile, ".formed");
  strcpy(lockfile, textfile);
  strcat(lockfile, ".lock");

  umask(18);

  /* if previous process is formatting same target text,
     wait for that process to finish formatting. */
  if (open(lockfile, O_CREAT|O_EXCL, 420) == -1)
    if (errno == EEXIST){
      i = 0;
      while (open(lockfile, O_CREAT|O_EXCL, 420) == -1){
        if (errno == EEXIST){
          sleep(1);
          /* if failed 20 times, then remove lockfile and exit abnormally */
          if (i++ == 20){
            unlink(lockfile);
            exit(1);
          }
        } else{
          perror(lockfile);
          exit(1);
        }
      }
      /* successfully formatted */
      unlink(lockfile);
      return 0;
    } else{
      perror(lockfile);
      exit(1);
    }
  else{
    /* format a text */
#ifndef NO_GREP
    tmpfname = tmpnam(NULL);
    unlink(formattedfile);
    sprintf(command, "%s -v '^[ \t]*$' %s | %s 's/\\([.?!;]\\) *$/\\1/' | %s 's/^  *\\(.*\\)$/\\1/' > %s",
            GREP, textfile, SED, SED, tmpfname);
    system(command);

    tmpfd = fopen(tmpfname, "r");
    fd = fopen(formattedfile, "w");

    while(fgets(line, 1024, tmpfd))
      fputs(line, fd);

    fclose(tmpfd);
    fclose(fd);
    unlink(tmpfname);
#else  /* !NO_GREP */
    unlink(formattedfile);
    if (!(txtfd = fopen(textfile, "r"))) {
          perror(textfile);
          exit(1);
        }
    if (!(fd = fopen(formattedfile, "w"))) {
          perror(formattedfile);
          exit(1);
        }
        while (fgets(line, 1024, txtfd)) {
          /* strip CR/LF */
      pl = line + strlen(line) - 1;
          while (*pl == '\n' || *pl == '\r') {
                *pl = '\0';
                if (pl > line) {
                  pl--;
                }
          }
          /* sed 's/\\([.?!;]\\) *$/\\1/' (Why TAB is neglected?) */
          while (pl > line && *pl == ' ') {
                pl--;
          }
      if (*pl == '.' || *pl == '?' || *pl == '!' || *pl == ';') {
                pl[1] = '\0';
          }
          /* sed 's/^  *\\(.*\\)$/\\1/' (Why TAB is neglected?) */
          for (pl = line; *pl == ' '; pl++) {
                ;
          }
          /* grep -v '^[ \t]*$' */
          if (!*pl) {
                continue;
          }
          fprintf(fd, "%s\n", pl);
        }
    fclose(txtfd);
    fclose(fd);
#endif /* !NO_GREP */

    /* release lock */
    unlink(lockfile);
    return 0;
  }
}
