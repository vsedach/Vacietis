; ================================================================
; Customization variables.

(defvar *system-include-directory* "ZETA-C:Include;"
  "The directory in which an #included file is looked for when the name is
   delimited with angle brackets, as in /"#include <stdio.h>/"; analogous to
   //usr//include on Unix.")

(defvar *system-include-sys-directory* #-TI "ZETA-C:Include;Sys;"
							    #+TI "ZETA-C:Include.Sys;"	  ;
  "The directory in which to look for a file included by a command of the form
   /"#include <sys/foobar.h>/".  See *SYSTEM-INCLUDE-DIRECTORY*.")
