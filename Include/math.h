/* -*- Mode: C; Tab-width: 5; Base: 10 -*-

	Copyright (C) 1986 by ZETA-SOFT, Ltd.
	All rights reserved.

 MATH.H for ZETA-C: declarations for mathematics functions.
 Note this file does not have a package specification, as it will be
 read into (potentially) many different packages. */

extern int atoi(), abs();
extern long atol();
extern double atof(), fabs(), floor(), ceil();
extern double exp(), log(), log10(), pow(), sqrt();
extern double sin(), cos(), tan(), asin(), acos(), atan(), atan2();
extern double sinh(), cosh(), tanh(), hypot();

/* Not defined, but declared: */
extern double fmod(), ldexp(), frexp();
extern double gamma(), j0(), j1(), jn(), y0(), y1(), yn();

#define HUGE 1.7014e38 /* a hair less than 2**127, max float */
#define LOGHUGE 39

extern int errno;
#define EDOM    33
#define ERANGE  34

/* End of MATH.H */
