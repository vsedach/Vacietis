/* -*- Mode: C; Tab-width: 5; Base: 10 -*-

	Copyright (C) 1986 by ZETA-SOFT, Ltd.
	All rights reserved.

 SIGNAL.H for ZETA-C: declarations for signal(), raise(), and assorted
 signals.  Modeled after UNIX and ANSI.

 Note this file does not have a package specification, as it will be
 read into (potentially) many different packages. */

/* Have to do sig_atomic_t */


#define  SIG_DFL	((int (*)()) 0)
#define  SIG_ERR	((int (*)()) -1)
#define  SIG_IGN	((int (*)()) 1)

/* These are UNIX v7 standard.  Of course, few are meaningful here. */
#define  SIGHUP	1
#define  SIGINT	2
#define  SIGQUIT	3
#define  SIGILL	4
#define  SIGTRAP	5
#define  SIGIOT	6
#define  SIGEMT	7
#define  SIGFPE	8
#define  SIGKILL	9
#define  SIGBUS	10
#define  SIGSEGV	11
#define  SIGSYS	12
#define  SIGPIPE	13
#define  SIGALRM	14
#define  SIGTERM	15

/* This is specified by the ANSI spec. */
#define  SIGABRT	SIGQUIT

void (*signal())();
int raise();


/* End of SIGNAL.H */
