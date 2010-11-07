/* -*- Mode: C; Tab-width: 5; Base: 10 -*-

	Copyright (C) 1986 by ZETA-SOFT, Ltd.
	All rights reserved.

 TIMES.H for ZETA-C: declarations for times() call.
 Note this file does not have a package specification, as it will be
 read into (potentially) many different packages. */

/* #include <sys/types.h>  first */

/* Only tms_utime is returned, and it has nothing to do with the current
 * process, but is global time; the others are zero */

struct tms {
	time_t tms_utime,		/* user time */
		  tms_stime,		/* system time (always zero) */
		  tms_cutime,		/* user time, children (always zero) */
		  tms_cstime;		/* system time, children (always zero) */
};

/* End of TIMES.H */
