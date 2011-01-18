/* update.c -- update High Score File
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
 * You'd need HAVE_STRING_H, HAVE_FCNTL_H, !HAVE_SYS_TIME_H, USE_TRRDIR.
 */
#else /* WIN32 */
#define DIR_SEP '/'
#endif /* WIN32 */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>

#if defined(HAVE_STRING_H)
#include <string.h>
#else
#include <strings.h>
#endif /* HAVE_STRING_H */  

#if defined(HAVE_SYS_TIME_H)
#include <sys/time.h>
#else
#include <time.h>
#endif /* HAVE_SYS_TIME_H */

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
  char scorefile[256], lockfile[256], datestr[64];
  char line[256], savedline[256];
  const char *user, *scores, *step, *times, *ttime, *token;
  FILE *fd, *tmpf;
  int score, tmpscore, i, myself, inserted;
  long datev;
#ifdef USE_TRRDIR
  char *env;
  char *pe;
#endif /* USE_TRRDIR */

  /* ignore signals */
#ifdef SIGHUP
  signal(SIGHUP, SIG_IGN);
#endif /* SIGHUP */
  signal(SIGINT, SIG_IGN);
#ifdef SIGQUIT
  signal(SIGQUIT, SIG_IGN);
#endif /* SIGQUIT */
  signal(SIGTERM, SIG_IGN);

  umask(18);
#ifndef USE_TRRDIR
  strcpy(scorefile, RECORD_DIR);
#else  /* !USE_TRRDIR */
  if (!(env = getenv("TRRDIR"))) {
#ifdef RECORD_DIR
    strcpy(scorefile, RECORD_DIR);
#else  /* RECORD_DIR */
    fprintf(stderr, "Can't get environment TRRDIR.\n");
    exit(1);
#endif /* RECORD_DIR */
  } else {
#ifdef WIN32
        for (pe = env; *pe; pe++) {
          if (*pe == UNIX_DIR_SEP) {
                *pe = DIR_SEP;
          }
        }
#endif /* WIN32 */
    sprintf(scorefile, "%s%crecord%c", env, DIR_SEP, DIR_SEP);
  }
#endif /* !USE_TRRDIR */

  /* create a new record file */
  if (argc == 2){
    strcat(scorefile, argv[1]);

    if ((fd = fopen(scorefile, "w")) == NULL){
      perror(scorefile);
      exit(1);
    } else
      fclose(fd);
    exit(0);
  }

  /* upfate high score file */
  strcat(scorefile, argv[1]);
  strcpy(lockfile, scorefile);
  strcat(lockfile, ".lock");
  user = argv[2];
  scores = argv[3];
  score = atoi(argv[3]);
  step = argv[4];
  times = argv[5];
  ttime = argv[6];

  time(&datev);
  strftime(datestr, 63, "%y.%m.%d, %H:%M", localtime(&datev));

  /* lock */
  i = 0;
  while (open(lockfile, O_CREAT|O_EXCL, 420) == -1){
    if (errno == EEXIST){
      sleep(1);
      /* if failed 20 times, remove lockfile and exit abnormally */
      if (i++ == 20){
        unlink(lockfile);
        exit(1);
      }
    } else
      exit(1);
  }

  if ((fd = fopen(scorefile, "r")) == NULL){
    perror(scorefile);
    unlink(lockfile);
    exit(1);
  }
  tmpf = tmpfile();
  inserted = 0;

  /* sorting ... */
  while (fgets(line, 256, fd)){
    myself = 0;
    strcpy(savedline, line);
    token = (char*)strtok(line, " \t");
    if (! strcmp(user, token))
      myself = 1;
    token = (char*)strtok(NULL, " \t");
    tmpscore = atoi(token);
    if ((! inserted) && (tmpscore <= score)){
      inserted = 1;
      if (strlen(user) < 8)
        fprintf(tmpf, "%s\t\t%s\t%s\t%s\t%s\t%s\n", user, scores,
                step, times, ttime, datestr);
      else
        fprintf(tmpf, "%s\t%s\t%s\t%s\t%s\t%s\n", user, scores,
                step, times, ttime, datestr);
    }
    if (! myself)
      fputs(savedline, tmpf);
  }
  if (! inserted){
    if (strlen(user) < 8)
      fprintf(tmpf, "%s\t\t%s\t%s\t%s\t%s\t%s\n", user, scores,
              step, times, ttime, datestr);
    else
      fprintf(tmpf, "%s\t%s\t%s\t%s\t%s\t%s\n", user, scores,
              step, times, ttime, datestr);
  }
  rewind(tmpf);
  fclose(fd);

  if ((fd = fopen(scorefile, "w")) == NULL){
    perror(scorefile);
    unlink(lockfile);
    exit(1);
  }
  while (fgets(line, 256, tmpf))
    fputs(line, fd);

  fclose(tmpf);
  fclose(fd);

  /* release lock */
  unlink(lockfile);
  return 0;
}
