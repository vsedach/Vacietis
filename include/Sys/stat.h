/*
 STAT.H for ZETA-C: declarations for stat(), and fstat();
 To be done soon??

 Note this file does not have a package specification, as it will be
 read into (potentially) many different packages. */



/* Getting the status of a file. */

struct stat
{
	dev_t  st_dev;
	ino_t  st_ino;
	unsigned short st_mode;				   /* Make this an int? */
	short  st_nlink;
	short  st_uid;
	short  st_gid;
	dev_t  st_rdev;
	off_t  st_size;
	time_t st_atime;
	time_t st_mtime;
	time_t st_ctime;
};

void stat();
void fstat();
