@Comment{-*- Mode: Text; Fonts: tr10, tr10i, tr10b, gach10, gach10b, gach10i; VSP: 0; Tab-width: 5 -*-

   ZETA-C User's Guide
   This document has been placed in the public domain.

This file is set up for the FinalWord Formatter, v2.0.  By now it is pretty totally wired in to the Apple Laserwriter.}
@Make(Manual)

@Style(TabSize 5)

@Define(st, font 12)
@Define(lt, font Helvetica250.bold)
@Define(it, font 12.italic)
@TCT("<<" "<<")
@TCT(">>" ">>")
@TCT(' "' "@case{font, 12, Helvetica250.bold ' "', else "@set(inquote 1)@char(0AAH)"}")
@TCT('("' "@case{font, 12, Helvetica250.bold '("', else "@set(inquote 1)(@char(0AAH)"}")
@TCT('"' "@case{font, 12, Helvetica250.bold '"', else "@set(inquote +1)@oddvalue(inquote,y "@char(0AAH)",n "@char(0BAH)")"}")
@TCT(" '" "@case{font, 12, Helvetica250.bold " '", else "`"}")

@Comment(Customizing manual for keyboard and other variables)
@String(Brand "TI")@Comment(or "Symbolics" or "LMI")
@String(Class "LMITI")@Comment(or "Symbolics")
@String(Machine "Explorer")@Comment(or "3600" or "LAMBDA")
@Case(Brand, LMI {@Form(BreakKey = "[Break]")@Form(SystemKey = "[System]")@~
			    @Form(RuboutHandlerTerm = "rubout handler")@Form(Qfasl = "@st<.QFASL>")@Form(CCFile = "cc-file")},
    TI {@Form(BreakKey = "[Break]")@Form(SystemKey = "[System]")@~
	    @Form(RuboutHandlerTerm = "rubout handler")@Form(Qfasl = "@st<.XFASL>")@Form(CCFile = "zeta-c:c-compile-file")},
    Symbolics {@Form(BreakKey = "[Suspend]")@Form(SystemKey = "[Select]")@~
			@Form(RuboutHandlerTerm = "input editor")@Form(Qfasl = "@st<.BIN>")@Form(CCFile = "zeta-c:c-compile-file")})
@String(ManualEdition "1.10")
@String(Release "1.1")
@String(System "TI Explorer Release 2.0")
@String(ZCSysVersion "15.0")

@Comment(Title page)
@PageFooting()
@Blankspace(1.33 in)
@MajorHeading{ZETA-C@+<(TM)> User's Guide}
@Blankspace(1 in)
@Center{This document has been placed in the public domain.

Manual edition @Value(ManualEdition)
for ZETA-C Release @Value(Release) (version @Value(ZCSysVersion))
running under @Value(System)

@Value(Day) @MonthName 19@Value(Year)}

@Comment(Inner title page)
@NewPage
@Begin(Text, Margins 1.75 in)
@Blankspace(2.5 in)

This document was formatted with the FinalWord(TM) v2.2 by Mark of the Unicorn, Inc., and printed on an Apple
LaserWriter(R).

Scott Layson Burson, the author of ZETA-C, wishes to acknowledge: the M.I.T. Artificial Intelligence Laboratory, for supporting
the original creation of Lisp Machines; the once-thriving Atari Cambridge Research Laboratory, for reintroducing him to
Lisp Machines and for providing an environment where the value of a C compiler for them could be clearly seen; LISP
Machines, Inc., for their generous assistance during the development of ZETA-C; and Thinking Machines Corp., especially
John Rose, for daring to be the first beta-test site.
@End(Text)


@Set(Page = 0)
@Comment{LaserWriter wizardry here, courtesy of BNH}
@Pageheading(left "ZETA-C User's Guide", right "@Value(ChapterTitle)",
line "@Escape(send ')p gsave 70 setlinewidth 1800 18675 moveto 13500 18675 lineto stroke 15 setlinewidth 1800 18787 moveto 13500 18787 lineto stroke grestore (')")
@Pagefooting(right "Page @Value(Page)",
line "@Escape(send ')p gsave 15 setlinewidth 1800 1800 moveto 13500 1800 lineto stroke grestore (')")
@String(ChapterTitle "Introduction")
@NewPage

@PrefaceSection(Introduction and obligatory hype)

Lisp Machines -- the Symbolics 3600(R) series, LMI LAMBDA(R), and TI Explorer(R) -- provide the best software
development environments in the world, but to date they have not been very useful for cross-development: it has been
difficult to take a program created and debugged on a Lisp Machine and transfer it to some other environment, as this
usually requires translating the program from Lisp to some other language.  Conversely, programs written in other
languages in other environments could not be run on a Lisp Machine.

ZETA-C(TM) helps bridge this portability gap by bringing C -- the popular systems language of UNIX(R) -- to the Lisp
Machine.  With ZETA-C, it is possible to develop programs in C and still make use of the power of the Lisp Machine
environment.

C, as traditionally implemented, is @i<unsafe>:@Index(safety of a C implementation) it is possible for an erroneous procedure
to damage data structures to which it is not intended to have access, typically by storing through an invalid pointer or
storing into an array at an invalid index.  Indeed, the greater part of debugging a C program often consists of tracking
down such errors.  Also, errors like these are often especially hard to find, as it will only be sometime later that the
damaged data structure will cause the process to crash, and by the time the crash finally comes, a lot of information
may have been lost that would be useful in diagnosing the error.

ZETA-C, on the other hand, is @i<safe>: the automatic array-bounds and pointer-validity checking which are built into the
Lisp Machine ensure that no procedure can damage a data sructure to which it doesn't explicitly have access.  An
attempt to store through an uninitialized pointer, or into an array at an invalid index (whether by way of a pointer or
an explicit @lt<array[index]> reference) will be trapped immediately, giving the user the full power of the Lisp Machine's
sophisticated debugger.  The user can then examine the context in which the damage was about to be done, rather than
the unrelated context in which it would, in the traditional scenario, have been discovered.  The time required to
diagnose the problem is thus reduced from hours or days to minutes.

Another time-saving feature of the Lisp Machine system is its facility for incremental compilation@Index(incremental
compilation) and dynamic linking.  Traditionally, when one makes a change to a single procedure, the entire file
containing the procedure must be recompiled, and the entire program (which may consist of several files) relinked, before
the change can be tested.  For even medium-sized programs, this debugging turnaround can easily run to 10 minutes.
Under ZETA-C, all of the "symbol tables" and intermediate data structures (which a traditional system must recreate
from scratch for every compilation and linking) are maintained incrementally; so only the procedures changed need be
recompiled, and there is no link phase.  The turnaround is thus a few tens of @i<seconds>, independent of the size of the
program.

ZETA-C programs live in the Lisp world, and can call Lisp functions and access some kinds of Lisp data structures
directly.  Thus, it is possible to incrementally convert a program from Lisp to C or vice versa.  For instance, one might
prototype a program in Lisp, then convert its modules one by one into C, testing the whole system as each module is
converted.  Conversely, it is easy to interface existing C programs to the Lisp world.

The price of the runtime array-bounds checking is, unavoidably, a certain performance penalty for array- and
pointer-intensive C code (and, of course, most C code is array- and pointer-intensive).  More subtly, algorithms are
written very differently in C than in Lisp, and Lisp Machine architectures are, obviously, optimized for the Lisp way
of doing things.  As a result, @Case(Machine, 3600 "a 3600/3670/3640", Explorer "an Explorer", LAMBDA "a LAMBDA")
runs a typical C program about @Case(Machine, Explorer "1/3 to 1/2", Symbolics "1/2 to 2/3", LMIT "1/3 to 1/2") the
speed of a VAX 11/750(R).  So we do not expect all CPU-intensive C programs to run acceptably under ZETA-C without
some hand-tuning.

Compatibility of ZETA-C with other C implementations is very good.  We have used as our reference @i<C: A Reference
Manual> by Samuel P. Harbison and Guy L. Steele Jr. (Prentice-Hall, 1984).  Chapter @Ref(IncompatibilitiesChapter)
documents the differences between ZETA-C and that standard.


@PrefaceSection(How to use this guide)

This User's Guide is organized into five chapters.  The first provides an overview of ZETA-C and some examples of its
use.  The second describes important details of the ZETA-C implementation and its interface to the Lisp Machine
environment.  The third discusses the extensions that have been made to Zmacs for editing C files.  The fourth describes
in detail the differences between ZETA-C and the Harbison & Steele standard.  (These first four are probably of immediate
interest to the first-time user or prospective purchaser.)  The fifth is a reference manual for the library of I/O routines
and "system calls".

@Begin(Comment)
Need a chapter for system installers/maintainers...  Needn't go into tremendous detail, but should list all the
customization variables and mention the possibility of locally different defaults...
Also, eventually, a chapter on how to use ZETA-C internals for analyzing C programs for your own purposes.
I was going to have a chapter summarizing portability and performance issues, but the portability part seems to have
been absorbed into the ZETA-C dialect chapter, and I don't think there's really a lot to say about performance.  (Even if
there is, I don't know what it is yet!)
@End(Comment)@~

This guide assumes throughout that the reader has some familiarity with the Lisp Machine software system and the
Common Lisp language.  It also assumes a working acquaintance with C.



@Chapter(Overview and examples)@Tag(HelloPage = Page)@Index(overview of ZETA-C)@Index(examples of, ZETA-C use)

ZETA-C is a fully @i<integrated> implementation of C within the Lisp Machine environment.  Its design philosophy has
been, not to "glue" a distinct C editing/compilation/execution system onto the side, as it were, of the Lisp Machine
software, but rather to @i<extend> the existing Lisp programming system to incorporate C.  Thus, Lisp and C functions can
freely call each other; Lisp can access all C data structures directly (no new fundamental datatypes have been created),
and C can access many Lisp data structures; C code, like Lisp code, is edited with Zmacs, to which commands specific
to C have been added; debugging is done with the same facilities; in fact, ZETA-C compiles a C function by translating it
into Lisp and handing the result to the existing Lisp compiler.

To see how all this works in practice, let's run through a simple example.  (This example is designed for you to follow
along at your own console, but it will still be comprehensible if you don't.  Following along, of course, assumes that
ZETA-C has been installed at your site.)  First, see if ZETA-C is loaded into your world; if not or if you don't know how
to tell, type at a Lisp listener @st<(make-system 'zeta-c :noconfirm)>.  Second, get a Zmacs window, and edit a new file
named "@st<hello.c>" (in your home directory).  As the first line of the file, type

@Begin(Display)
@st</* -*- Mode: C; Package: (hello C) -*- */>
@End(Display)

Issue the Zmacs command @lt<meta-X Reparse Attribute List>; you will notice that the Zmacs mode line now says
@NoTCT<@lt<(C)>>, indicating that C mode is active, and the wholine at the bottom of the screen now shows the current
package as @lt<HELLO:>.  Next, enter the following C function.

@Begin(Display)@Index(example of, hello world)
@st<main()
{
     printf("\nHello, world!\n");
}>
@End(Display)

Now give the Zmacs command @lt<control-shift-C>.  You will see in the echo area, first @Case(Class, LMITI
{"@st<C-compiling MAIN>"}, Symbolics {"@st<Compiling MAIN>"}), then "@st<-- compiled.>"  Now get to a C listener
by typing @st<@BreakKey>, and type "@st<main();>".  You will see@Index(example of, C listener)

@Display{
@st<Hello, world!>
}

Type @st<[Resume]> or @st<[Abort]> to return to Zmacs.  Now let's save this program as a file and compile it to produce
a @Qfasl file.  Save the file with @lt<control-X control-S>, then select a Lisp listener (with @st<@SystemKey L>) and
type the form

@Display{@st<(@CCFile "@it<directory> hello.c")>}
@NoHinge
where @it<directory> is the name of your home directory.  The value returned will be the pathname of the @Qfasl file;
type "@st<(load *)>" to load it, and "@st<(hello:|main|)>" to run the program.  Note the vertical bars around the symbol @st<main>.
These are necessary to suppress the normal conversion of letters to uppercase.@Foot{The Lisp reader's uppercase
conversion allows the strings "@st<foo>", "@st<Foo>", and "@st<fOo>" (for example) all to be read as the symbol @st<FOO>; so Lisp code can
normally be written in upper, lower, or mixed case.  C, on the other hand, requires that such strings be read as distinct
identifiers.} ZETA-C,@Index(identifiers)@Index(case of identifiers) unlike Lisp, is @i<case-sensitive>, and will not recognize
"MAIN" or "Main" as a version of "main".  Instead these are three different symbols, and in our example, only the last
one has been defined.  Be careful @i<not> to extend the upright bars around the package prefix, or Lisp will not recognize it
as such.  If you wish to avoid using upright bars when calling C from Lisp, simply use all-uppercase names in your C
code.

Now let's try a fancier example.  Still in Zmacs, find the file "@st<zeta-c:source;turtle.c>".@Index(turtle graphics) Give
the command @lt<meta-X @Case(Class, LMITI "C ")Compile Buffer>@Case(Brand, Symbolics { (you will see a message like
"@st<Warning: the package TURTLE failed the validation function; the standard value for it will not be
changed.>"; ignore it)}).  When the compilation is complete, again hit @st<@BreakKey>, and type "@st<init();>".  The mouse cursor
will change to an inverted-L shape, indicating that you are being asked to designate the corners of a window.  Make a
small (2") square window which does not overlap with the editor window (this may not be possible, in which case after
you make the small window you should use the Edit Screen option on the system menu to reshape the editor window so
it does not overlap the new window; in this case type "@st<init();>" again after you uncover the new window).  You will
see a small triangle (the "turtle") in the center of your window.  Those of you who have ever played with the kids'
programming language Logo will recognize the commands available:

@Begin(Description, fill n, spread 0 lines, indent -1.2 in)
@st<fd(@it<dist>);>@\Move the turtle forward @it<dist> pixels, drawing a line.

@st<bk(@it<dist>);>@\Move the turtle back @it<dist> pixels, drawing a line.

@st<rt(@it<angle>);>@\Turn the turtle right by @it<angle> degrees.

@st<lt(@it<angle>);>@\Turn the turtle left by @it<angle> degrees.

@st<pu();>@\("Pen Up")  Raise the turtle's "pen", so that it does not leave a line when it moves.

@st<pd();>@\("Pen Down")  Lower the turtle's "pen", so that it @i<does> leave a line when it moves.
@End(Description)

These primitives are sufficient for drawing many kinds of pictures, from simple to quite complex.  You can define
procedures using the C listener, just by typing in the definition as if you were entering it into a C source file.  Try, for
instance, defining a procedure that draws a square of specified size:

@Begin(Display)@Index(example of, C listener)
@st<square(len) int len; { int i; for (i = 0; i < 4; ++i) { fd(len); rt(90); } }>
@End(Display)

Call it with various arguments.  (You may notice that round-off error causes the squares to be slightly distorted.
Rewriting the arithmetic in @st<turtle.c> to prevent this is left as an exercise for the reader.)  Write another procedure that
displays several squares of different sizes and/or at different orientations or origins.  Play!  The C listener is a very
important tool for interacting with your ZETA-C program, and it is worthwhile to spend a few moments simply getting
comfortable with it.

@Begin(Comment) I want to print this now...
[[Other examples to go in here: how to set up a full-blown C program, with @lt<argc>, @lt<argv>, and a current
directory; how to set up a @lt<defsystem> for a C program. -- Is this stuff really needed here?]]
@End(Comment)


@Chapter(The ZETA-C Implementation)@Index(ZETA-C implementation)@Index(implementation)

This chapter discusses details of the ZETA-C implementation and its interface to the Lisp Machine environment, both in
terms of the user side (how one goes about compiling and running C programs) and the Lisp side (how one connects Lisp
programs to C programs).  Because of the open nature of the Lisp Machine software system, these two aspects are often
deeply intertwined.


@Section(Packages)@Tag(PackagesPage = Page)

It is important to understand how ZETA-C makes use of Lisp @b<packages>.@Index(package, C:)@Index(package, C program)
(The following discussion assumes an understanding of the package system; if you are not familiar with its use, you
should read the appropriate section of the Lisp Machine documentation).  It is important that names (of variables,
functions, etc.) in a C program be kept distinct from those in the Lisp world as well as those in other C programs.  For
instance, the ZETA-C user must be allowed to define a function @lt<car> without it conflicting with the Lisp function of
that name.  To accomplish this, first, ZETA-C itself defines a package @lt<C:> which @i<does not> inherit from the @lt<GLOBAL:>
package; then, users define packages which inherit from @lt<C:> in which to intern their programs (we will call these latter
"C program packages").

@Case(Class, Symbolics {The Standard Value system@Index(standard value)@Index(validation function) checks,
whenever you enter a Lisp Listener (by entering the Debugger because of an error, or by hitting @st<[Suspend]>), that the
current package inherits from @lt<GLOBAL:>; if it doesn't (no C program package does) then it will choose some other package
-- often @lt<USER:> -- to make current.  So on entering the debugger you will see a message like "@st<Binding> @st<PACKAGE to
#<Package USER 16600000> (old value was> @st<#<Package CPROG 20560347>).>"  So when you're in the Debugger or a
Break loop, you will have to type an explicit package prefix to access symbols in your C program package.

Also, in ZMACS, whenever you select a buffer in C mode, you will see a message like "@st<Warning: the package CPROG
failed the validation function; the standard value for it will not be changed.>"  These messages can be
ignored.}, LMITI {A consequence of this arrangement is that when one is typing Lisp forms at a Lisp listener or the
Debugger, and the current package (the value of the system variable @lt<package>) is a C program package, explicit
package prefixes are necessary to access the built-in Lisp functions, as they are all kept in @lt<GLOBAL:>.  This takes some
getting used to.  To save typing, notice that the nickname @lt<GL:> is available for @lt<GLOBAL:>.  So, for instance, if
the current package is a C program package -- say, @lt<CFOOD:> -- and you wish to inspect one of the program's data
structures -- say the value of @lt<frobarray> -- you can type

@Display{@st<(gl:inspect frobarray)>}

However, to do the same thing when the current package is @lt<USER:> (or some other non-C package), type

@Display{@st<(inspect cfood:frobarray)>}})@Comment(End of @Case)

Of course, what usually happens if one omits a needed package prefix is an undefined-function or unbound-symbol
error, which can easily be fixed either with one of the fancy Debugger options or by simply hitting @st<[Abort]> and
retyping the form.

See p. @Ref(ProgramConstructionPage) to see how to create a C program package.

C identifiers may also contain package prefixes@Index(package, prefix)@Index(example of, package prefix), delimited
with the "@st<$>" character (which is otherwise unused in C).  So, for instance, the C statement "@st<TV$BEEP();>", equivalent to
the Lisp "@st<(tv:beep)>", would beep the console beeper (and/or flash the screen).


@Section(Data structures)@Tag(DataStructuresPage = Page)@Index(data structures)@Index(C data structures)

To interface C and Lisp code, or just to understand some of the performance issues inherent in ZETA-C, it is very
handy to know how ZETA-C implements C data structures in terms of those provided by Lisp.


@SubSection(Numbers)@Tag(NumbersPage = Page)@Index(numbers)@Index(integer types)@Index(character types)

ZETA-C does not make any distinction between types @lt<int> and @lt<long>, or between @lt<unsigned int> and @lt<unsigned long>.
These are all implemented with the numeric type most natural to the Lisp Machine, namely arbitrary-precision
integers.@Index(arbitrary-precision integer)@Index(integer, arbitrary-precision)@Index(precision, integer)@Index(word
length)@Index(representation, integer) We do not restrict the widths of these types to, say, 32 bits, because doing so
would make ZETA-C object code decidedly @i<slower>.@Index(performance issues)

The treatment of unsigned arithmetic@Index(integer, unsigned) in ZETA-C was problematic.  What should be the value,
for instance, of @lt<(unsigned)-1>?  Since the "word length" of arbitrary-precision integers is effectively infinite, there is no
particular positive integer that it makes sense to use in this case.  What we do is this@Foot{I am indebted to John Rose
of Thinking Machines for this solution.}: we use the negative fixnum @lt<-1> as a representation of the unsigned value
@lt<@Infinity-1>, and so on.  The only operation that's actually affected by this interpretation is that of comparison: we
simply arrange that any unsigned value implemented as a negative integer is treated as @i<greater> than any value
implemented as a positive integer.

The types @lt<short>@Index(integer, short) (equivalent to @lt<signed short>) and @lt<unsigned short> are truncated to 16 bits.
This truncation is inefficient on the Lisp Machine, and so for optimum performance@Index(performance issues) we do
not recommend the use of the short types for variables or structure components; arrays of them work better (see below).
Similarly for @lt<char>@Index(character) (equivalent to @lt<unsigned> @lt<char>) and @lt<signed> @lt<char>, which are truncated to 8 bits.
The @lt<char> type is unsigned by default not only because of this efficiency difference, but also because C requires that all
characters in the host character set be represented by positive values of @lt<char> variables, and the Lisp Machine character
set defines several control characters above 0200 octal.

Variables of all integer types are stored so that their actual values are equal to their "apparent" values (as seen from
Lisp).  This means that any value greater than @Case(Class, Symbolics "2@+<31>-1", LMITI "2@+<24>-1") will require a
bignum to represent it.

@Tag(FastFloatPage = Page)
@Case(Class, Symbolics {The @lt<float> type@Index(floating-point precision)@Index(precision, floating-point)
@Index(representation, floating-point) is implemented with single-precision floating-point numbers, which have a 24-bit
mantissa and an 8-bit exponent; @lt<double> uses double-precision (53-bit mantissa, 11-bit exponent).  Double-precision
arithmetic is rather less efficient than single-precision -- not just because the computation takes longer, but also because
double-floats are consed in the heap rather than fitting in a pointer, as single-floats do -- so ZETA-C provides an option
to suppress the automatic conversion of @lt<float>s to @lt<double>s before performing any arithmetic.  Just put the line
@Display{@st<#define ZETA_C_SUPPRESS_AUTO_FLOAT_TO_DOUBLE>} at the beginning of your program.}, LMITI {The @lt<float>
type@Index(floating-point precision)@Index(precision, floating-point)@Index(representation, floating-point) is
implemented with small-floats, which have an 18-bit mantissa and a 7-bit exponent; @lt<double> uses single-floats (31-bit
mantissa, 11-bit exponent).  Single-float arithmetic is rather less efficient than with small-floats, because single-floats
are consed in the heap rather than fitting in a pointer, as small-floats do; so ZETA-C provides an option to suppress the
automatic conversion of @lt<float>s (small-) to @lt<double>s (single-) before performing any arithmetic.  Just put the line
@Display{@st<#define ZETA_C_SUPPRESS_AUTO_FLOAT_TO_DOUBLE>} at the beginning of your program.})


@SubSection(Pointers)@Tag(PointersPage = Page)

The choice of representations for pointers@Index(pointer representation)@Index(representation, pointer) is constrained on
the one hand by the ways that pointers can be used in C programs, and on the other by the design goal that ZETA-C be
a @i<safe> implementation.  For instance, on the one hand, it is necessary that pointer arithmetic be possible: that one be
able to create a pointer to an element of an array, and then add an integer to that to get a pointer to a different
element; on the other hand, if in doing this one creates a pointer to a non-existent element, an attempt to assign or
reference through that pointer must be trapped.

To satisfy these constraints, it is clearly insufficient that a pointer be, as in C, merely an address.  The system must
keep track of the array-bounds information that goes with the address.  We accomplish this by representing a pointer as
a pair <@i<array>, @i<index>>, where the @i<array> is a Lisp array object representing a C array or structure (see the next section),
and the @i<index> specifies an element thereof.  How the pointer is actually represented depends on where it is stored.
Pointer variables are implemented as pairs of Lisp variables, named @i<ptr@lt<.array>> and @i<ptr@lt<.index>>,@Index(@st<.array> and
@st<.index> variables) where @i<ptr> is the name of the C pointer variable.  Pointers as array or structure elements take up two
consecutive elements of the Lisp array; the first is @i<array> and the second is @i<index>.  These implementation matters are of
course entirely invisible to C programs, provided it is not assumed that a pointer and an @lt<int> are the same size.

When a pointer is passed to a function, it is passed as two consecutive arguments@Index(@w<arguments, pointer>); just
like a pointer variable, a pointer parameter in a function definition turns into a pair of parameters, which appear
consecutively in the lambda-list.  A pointer is returned from a function as two values, the array and
index@Index(@w<return value, pointer>).  Again, this is completely invisible to C code, provided all functions that
accept and return pointers are correctly declared.

In order to provide for the creation of pointers by taking the address (unary @lt<&>) of scalar variables (as opposed to
elements of aggregates), ZETA-C creates "address arrays".@Index(address array)@Index(pointer to variable) The address
array of variable @i<var> is kept in a variable @i<var@lt<.address>>.  (Address arrays are always created for external and static
variables, but only if needed for automatic variables.)  So, a pointer to @i<var> is the pair <@i<var@lt<.address>>, @st<0>>.

An example should make all of this clear@st<.>  The C function@Index(example of, pointer implementation)
@Begin(Display)
@st<gubbish(ipp)
     int **ipp;
{
     static int i;
     int *ip;
     ip = &i;
     *ipp = ip;
     frobozz(ip);
     return ip;
}>
@End(Display)
would be implemented in Lisp something like this (much irrelevant detail, notably the initialization of @st<i> and
@st<i.address>, has been omitted):
@Begin(Display)
@st<(defun gubbish (ipp.array ipp.index)
  (let ((ip.array nil) (ip.index 0))
    (setq ip.array i.address)
    (setq ip.index 0)
    (setf (aref ipp.array ipp.index) ip.array)
    (setf (aref ipp.array (1+ ipp.index)) ip.index)
    (frobozz ip.array ip.index)
    (values ip.array ip.index)))>
@End(Display)

Some obvious consequences of these decisions: to increment a pointer we increment its index part; two pointers are equal
iff their array parts are @lt<eq> and their index parts are equal; the difference of two pointers is the difference of their index
parts (assuming they point into the same array; an error occurs otherwise), scaled down if necessary by the size of the
objects they point to.

Special cases: the null pointer (of any type) is represented as having array part @lt<nil> and index part @lt<0>.  Any attempt to
dereference it will of course cause an error to be signalled, since @lt<nil> is not an array.  A
pointer-to-function@Index(representation, function pointer) is the exception to @i<all> of this -- it's not a pair, it's just a
single value: the symbol that names the function.  (A null function pointer is the symbol
@lt<zeta-c:null-function-pointer>@Index(@st<zeta-c:null-function-pointer>)@Index(@st<null-function-pointer>).)


@SubSection(Arrays, structures, and unions)@Tag(AggregatesPage = Page)@Index(aggregates)@Index(arrays)
@Index(structures)@Index(unions)@Index(packed_struct)

C arrays are Lisp arrays.@Index(representation, array) Since C and Lisp use the same bounds convention -- an array of
size @i<n> contains elements numbered 0 through @i<n>-1, inclusive -- no subscript translation is necessary.

The representation chosen for an array depends on the type of the elements to be stored in it.  Arrays of @lt<int>, @lt<long>,
@lt<unsigned int>, @lt<unsigned long>, any kind of pointer, or of structures or unions are implemented with Lisp array type
@lt<art-q>.  Arrays of @lt<short> and @lt<unsigned short> are implemented as @lt<art-16b> arrays; arrays of @lt<char> and @lt<signed char> are
implemented as @lt<art-8b> arrays.  There is no extra cost associated with using these to hold unsigned values, since that is
the interpretation the microcode imposes, but @lt<signed char>s and (@lt<signed>) @lt<short>s must be sign-extended after being loaded
from an array; this renders the use of the signed types slightly slower.

Lisp Machines are fundamentally word-addressed machines.  However, in order to accomodate programs that take, for
instance, a pointer to @lt<short>, cast it to a pointer to @lt<char>, store two characters at successive locations through that
pointer, and then expect both characters to have fit in a single @lt<short> -- a nonportable but all too common practice --
ZETA-C simulates byte-addressing by means of displaced arrays.  To continue this example, consider a pointer, of type
"pointer to @lt<short>", which at runtime points to element @i<i> of an array @i<a16b> of type @lt<art-16b>.  Casting this pointer to a
@lt<char>-pointer causes the creation (at runtime) of an @lt<art-8b> array @i<a8b>, whose length is twice that of @i<a16b> and whose
contents are displaced to overlay those of @i<a16b>; the result of the cast is a pointer to element 2@i<i> of @i<a8b>.  If one stores
characters through successive values of this pointer (i.e., to elements 2@i<i>, 2@i<i>+1, ... of @i<a8b>) and then looks at the result
through the original @lt<short>-pointer, the characters will be seen to have been stored two-per-halfword starting at the
low-order end of the halfword.

@Case(Class, Symbolics {Types stored in @lt<art-q> arrays can take part in this pointer casting also.  However, there is a
slight complication, since @lt<art-q> arrays can hold non-numeric objects, and it would not make sense to load or store bytes
or halfwords out of or into a Lisp object pointer.  Fortunately, the 36xx microcode takes care of this case very nicely.  If
you try, for instance, to load a byte out of a word that contains the array part of a ZETA-C pointer, you will get an
error to the effect that @st<The AR-1 microcode encountered an ARRAY-WORD that was not a fixnum>.  One way to do
this is (you can try this in a C listener):
@Begin(Display)
@st<char *cp, *cparray[1] = { "Hello there" };
cp = (char *)cparray;
*cp;>
@End(Display)}, LMITI {While this pointer casting works for @lt<char> and @lt<short> types, it doesn't work with @lt<int> or other
types stored in @lt<art-q> arrays.  The problem is that since @lt<art-q> arrays can hold any Lisp object, they must be stored in
"boxed" or tagged form, wherein the high-order 7 bits of each word hold a type code.  An @lt<art-8b> displaced onto an
@lt<art-q> would allow the inadvertent modification of those bits, which could easily crash the Lisp garbage collector.  Also,
the high-order byte of an integer would appear to contain an incorrect value.  So if you cast an @lt<int> pointer to a @lt<char>
pointer, the two will @i<not> point to the same storage (although if you cast the result back to an @lt<int> pointer, you will get
the same thing you started with).})

Structures are represented as @lt<art-q> arrays@Index(representation, structure); the elements of the structure occupy
successive elements of the array.  ZETA-C, like traditional C implementations, uses a "flat" representation for nested
aggregates, such that the storage occupied by an inner aggregate is part of that occupied by the outer one.  So, for
instance, the declaration@Index(example of, nested aggregates)
@Begin(Display)
@st<struct foo {
     int foovals[3];
     struct foo *nextfoo;
     } fooarray[20];>
@End(Display)
allocates a single @lt<art-q> array of length 100 (remember, a pointer takes up two cells).  Furthermore, arrays of @lt<short>s or
@lt<char>s within a struct will be implemented using the same displaced-array machinery that handles pointer casts; so
the declaration@Index(example of, struct implementation)
@Begin(Display)
@st<struct bar {
     int a;
     struct foo *foop;
     char name[20];
     } abar;>
@End(Display)
allocates an @lt<art-q> of length 8: 1 for @lt<a>, plus 2 for @lt<foop>, plus 5 for @lt<name>.  It also creates and caches (see below) an
@lt<art-8b> of length 32, @Case(Class, Symbolics "displaced onto the @lt<art-q,> ")so that @lt<name> is accessible as elements 12
through 21 thereof.  @Case(Class, LMITI {Thus every struct has both a "boxed" or @lt<art-q> part, and an "unboxed" part
consisting of an @lt<art-16b> and an @lt<art-8b> that share storage.})

Normally, ZETA-C uses an "unpacked" representation for structures, in the sense that all scalar elements of numeric
type are allocated an entire word each.  (Arrays within structs are always packed, as we have just discussed.)  So, for
instance, the declaration
@Begin(Display)
@st<struct zot {
     unsigned short z1;
     char z2, z3;
     struct foo *foop;
     } azot;>
@End(Display)
creates an @lt<art-q> of length 5, not 3 as one might imagine (expecting @st<z1>, @st<z2>, and @st<z3> all to fit in the first cell).  This is
done for performance reasons: we don't want waste a lot of time switching representations to load structure elements.  If
for some reason it's important to your application that structs be "packed" (so that @st<z1>, @st<z2>, and @st<z3> all @i<would> fit in the
first cell and the total length @i<would> be 3), use the keyword @lt<packed_struct>@Index(representation, packed structure) in
place of @lt<struct> for declaring all structs that need to be packed.  (@lt<packed_struct>s can be nested inside @lt<struct>s and
conversely, with complete freedom, though we can't imagine why this would be useful.)

The implementation of unions is very much like that of structures@Index(representation, union).  A union of aggregates
is an @lt<art-q> array whose size is the size of the largest aggregate in the union, and each of whose elements can be
referenced as the corresponding element of any of the aggregates.  For example, given the declaration
@Begin(Display)@Index(example of, union implementation)
@st<union point {
     struct rect_pt {
          float x, y;
          } rp;
     struct polar_pt {
          float r, theta;
          } pp;
     } pnt;>
@End(Display)
@st<pnt> will be an array of length 2; @st<pnt.rp.x> and @st<pnt.pp.r> both refer to element 0 of this array; @st<pnt.rp.y> and
@st<pnt.pp.theta> both refer to element 1.

When ZETA-C allocates a Lisp array to represent a C aggregate, it sets it up with a
named-structure-symbol@Index(named structure symbol) and other information in the array-leader so the Lisp printer
will print it recognizably; e.g., the declaration @DisplayOne{@st<char buffer[256];>} creates an array that will print as
@st<@w<#{(char [256]) BUFFER}>>.  Slots are also allocated in the array-leader to cache the displaced-arrays created by
pointer-casting.  Specifically, here is how ZETA-C uses each element of the array-leader:@Index(@w<leader slots,
reserved>)@Index(array leader)

@Begin(Text, Indent -6)
0:@\Normally, a list (@i<name type env>), where @i<name> is the name with which the aggregate was declared (or @lt<nil> if it was
dynamically allocated), and @i<type> and @i<env> are the ZETA-C internal representations for the type of the aggregate and the
contents of the environment in which it was declared, respectively.  However, if the named-structure-symbol is
@lt<zeta-c:cast-array>, element 0 contains the original array that the cast-array is displaced to; element 0 of @i<that> array will
contain this descriptor-list.

1:@\The named-structure-symbol.  For a structure or union whose type was declared with a tag, the tag is used as the
named-structure-symbol.  Otherwise, there appears one of the symbols @lt<zeta-c:array>, @lt<zeta-c:struct>, @lt<zeta-c:union>,
@lt<zeta-c:value-cell> (used for address arrays of variables), @lt<zeta-c:cast>-@lt<array> (used for arrays, created by casting, that
are displaced onto other arrays, as discussed above), or @lt<zeta-c:restarg> (used internally similarly to @lt<&rest> arguments
in Lisp; see page @Ref(varargsPage)).

2:@\Reserved for ZETA-C internal use.

3:@\When an @lt<art-q> cast-array for this aggregate is created, it is cached here; otherwise @lt<nil>.

4:@\Cache for the @lt<art-16b> version.

5:@\Cache for the @lt<art-8b> version.
@End(Text)

Note that one of the last three slots will always contain the array itself, since it has to be in one of the three
representations.


@SubSection(Strings)@Index(strings)

String constants in C code are written, of course, according to the standard C conventions@Index(representation, string).
However, if you look at a string in the C listener, you will see that it doesn't look quite the same when printed out as it
does in your code.  The special @lt<\>-sequences will have been converted to the appropriate single characters, and a @lt<NUL>
character (which displays as a raised dot, @Dot) will have been appended.  So, for instance:

@Begin(Display)
What you write in your C code:
@st<@NoTCT<"Hello, world!\n">>

What is printed:
@st<@NoTCT<"Hello, world
@Dot">>
@End(Display)

Note that if you pass a Lisp string to a C function, you must be sure to have appended the @lt<NUL> yourself.  (If you don't,
the C function will get an array-bounds error when it scans off the end of the string, since this is the only way C code
has of knowing where a string ends.)  Conversely, for Lisp code to make proper use of a C string, the @lt<NUL> must be
stripped and the character-pointer converted to a Lisp string.  The following functions exist for this purpose.

@FunctionDoc(Name "@lt<zeta-c:string-to-C>@Index(@st<zeta-c:string-to-C>)@Index(@st<string-to-C>)@Index(converting
strings between C and Lisp)", Arglist "@it<str>", Doc "Given a Lisp string, returns a C character pointer (as
two values: array and index) that points to the beginning of a copy of the string.  Appends a NUL to the copy.")

@FunctionDoc(Name "@lt<zeta-c:string-to-lisp>@Index(@st<zeta-c:string-to-lisp>)@Index(@st<string-to-lisp>)", arglist
"@it<str.array str.index> &optional@it< case>", Doc "Given a C character pointer <@it<str.array>, @it<str.index>>, returns a copy of
its contents as a Lisp string.  @it<case>, if supplied, may be @lt<:upcase> or @lt<:downcase>, requesting a forced conversion to upper
or lower case respectively.")

The Lisp Machine's character set@Index(character set) is unusual in that the first 128 characters (0 through 177 octal)
are all printing characters; the control characters are octal 200 through 237.  See your Lisp manual for more details.  As
long as you use the predefined escape sequences @lt<\r>, @lt<\n>, @lt<\b>, @lt<\t>, @lt<\f>, and @lt<\v>, this will not make any difference to your
programs, unless you are using the high-order bits of characters for some special purpose.  The sequences @lt<\r> and @lt<\n> both
name the @st<[Return]> character, octal 215; @lt<\b> is @st<[Overstrike]>, octal 208; @lt<\t> is @st<[Tab]>, octal 209; and @lt<\f> and @lt<\v> are both
@st<[Page]>, octal 214.
@Comment(More in ANSI spec?)


@Section(Debugging hints)@Index(debugging)

This section contains explanations of the error messages@Index(error messages) issued by ZETA-C, along with some
material that will help you interpret them and find the problem efficiently.


@SubSection(Syntax errors)@Index(errors, syntax)

When the ZETA-C parser encounters a syntax error, it displays three lines of context with a marker at the point where
the error was detected, like this:

@Begin(Display)
@st<<< Error in reading >>
 Error while parsing line 58 of ED-BUFFER: CTEST.C#> GYRO.ZETA-C; ASTARTE::
Expression syntax
Error happened somewhere before the point indicated by> @st<"->HERE<-" in:
{
     *junkp += c
} ->HERE<- >
@End(Display)

In this case, as you see, the error was a missing semicolon.  Here are the error messages the parser can emit whose
interpretations may not be obvious:

@ErrorDoc(Message "@st<Expression syntax>", Doc "An error was found at the expression level.  Look for a missing semicolon,
unbalanced parentheses, or the like.")

@ErrorDoc(Message "@st<External definition syntax>", Doc "An error was found in an external variable declaration or
function definition.  Look for a mistyped type name, a missing semicolon or comma, or mismatched delimiters.")

@ErrorDoc(Message "@st<Statement syntax>", Doc "An error was found at the statement level.  Look for an incorrectly written
@lt<if>, @lt<for>, @lt<while>, etc.")

When a syntax error occurs, the parser will attempt to recover and continue.  In the case of something simple, like a
missing semicolon, this will usually work, but many errors will throw the parser off completely.  When this happens,
there are likely to be more syntax errors; just fix the first error and the rest will probably go away by themselves.


@Subsection(Compilation errors)@Index(errors, compilation)

When an error is encountered in semantic analysis or "code generation", a message is issued which displays the
offending structures in ZETA-C's internal representations.  So, clearly, in order to be able to fully understand such a
message, one should know how to interpret those representations.@Foot{This is not a deficiency on ZETA-C's part
vis-a-vis other compilers, since the latter don't have such detailed error messages in the first place.}  There are two
important representational systems: that used for the parsed input expressions, and that used to describe the types of
values.


@Paragraph(Representation of expressions)@Index(representation, expression)@Index(expressions)

Expressions (and statements) are represented quite straightforwardly as Lisp forms; for instance, the C statement
@Display{@st<a = b + 3*foo((c == 0) ? d : e) + f[g++];>}
is represented as the Lisp form
@Display{@st<(= A (+ B (+ (* 3 (FOO (|?:| (== C 0) D E))) ([] F (X++ G)))))>}

From this example, several points should be visible immediately:

@Begin(Itemize)
Operators have names which are identical to, or at least strongly suggestive of, their C notations.

Calls to user functions appear just as they do in Lisp.

Variable names are converted to upper case (see p. @Ref(IdentifiersPage)).@Comment{This will change!}
@End(Itemize)

@Begin(Figure)
@Begin(Comment) I don't want to lose the old font information...
@Begin(Description, fill 0, indent -1 in)
@lt<+>@\Addition (one argument may be a pointer).
@lt<->@\Subtraction (one or both arguments may be pointers).
@lt<*>@\With one argument, pointer dereferencing; with two, multiplication.
@lt</> (@lt<//>)@Foot{Slashified representations are given in parentheses when appropriate.}@\Division.
@lt<%>@\Remainder.
@lt<<<>@\Shift left.
@lt<>>>@\Shift right.
@lt<&>@\Bitwise AND.
@lt<|> (@lt</|>)@\Bitwise OR.
@lt<^>@\Bitwise XOR.
~@\Bitwise NOT (one argument).
@Hinge@Blankspace(1 line)
@lt<==>@\Equality comparison.
@lt<!=>@\Inequality comparison.
@lt<<>@\Less-than comparison.
@lt<>>@\Greater-than comparison.
@lt<<=>@\Less-than-or-equal comparison.
@lt<>=>@\Greater-than-or-equal comparison.
@lt<!>@\Logical NOT.
@lt<&&>@\Logical AND.
@lt<|| >(@lt</|/|>)@\Logical OR.
@Hinge@Blankspace(1 line)
@lt<=>@\Assignment.
@lt<++x>@\Preincrement.
@lt<x++>@\Postincrement.
@lt<@NoTCT<--x>>@\Predecrement.
@lt<@NoTCT<x-->>@\Postdecrement.
@lt<+=>@\Add and assign (first argument may be a pointer).
@lt<-=>@\Subtract and assign (first argument may be a pointer).
@lt<*=>@\Multiply and assign.
@lt</=> (@lt<//=>)@\Divide and assign.
@lt<%=>@\Take remainder and assign.
@lt<<<=>@\Shift left and assign.
@lt<>>=>@\Shift right and assign.
@lt<&=>@\Bitwise AND and assign.
@lt<|=> (@lt</|=>)@\Bitwise OR and assign.
@lt<^=>@\Bitwise XOR and assign.
@Hinge@Blankspace(1 line)
@lt<[]>@\Array reference.
@lt<.> (@lt</.>)@\Structure element reference.
@lt<@NoTCT{->}>@\Indirect structure element reference.
@Hinge@Blankspace(1 line)
@lt<progn+>@\The comma operator (which acts like the Lisp @lt<progn>).
@lt<?:> (@lt<?/:>)@\The expression conditional.
@Hinge@Blankspace(1 line)
@lt<if>@\The statement conditional.
@lt<block+>@\Encloses statements in a block (see text).
@lt<goto>@\Go to the specified label.
@lt<label+>@\The first argument is a label; the second is a statement.
@lt<while>@\@st<(while @i<looptest> @i<body>)>: iterate.
@lt<do>@\@st<(do @i<body> @i<looptest>)>: iterate, doing @i<body> at least once.
@lt<for>@\@st<(for @i<init> @i<looptest> @i<increment> @i<body>)>: iterate with syntactic sugar.
@lt<break>@\Skip to the end of this @lt<while>, @lt<do>, @lt<for>, or @lt<switch>.
@lt<continue>@\Skip to the next iteration of this @lt<while>, @lt<do>, or @lt<for>.
@lt<return>@\Return from this function (argument, if any, is value to return).
@lt<switch>@\@st<(switch @i<exp> @i<body>)>: go to the @lt<case> in @i<body> matching @i<exp>.
@lt<case>@\@st<(case @i<value>)>: meaningful only in @lt<switch> bodies.
@Hinge@Blankspace(1 line)
@lt<cast+>@\@st<(cast>+@st< @i<type> @i<expression>)>: cast the type of @i<expression> to @i<type>.
@lt<sizeof>@\The size of a type or instance.
@lt<#lisp> (@lt</#lisp>)@\Encloses a group of Lisp forms introduced with @lt<#lisp> (see p. @Ref(#lispPage)).
@lt<salloc>@\Allocate a structure or union.
@lt<aalloc>@\Allocate an array.
@Hinge@Blankspace(1 line)
@lt<defunc+>@\Function definition (see text for explanation).
@lt<decl+>@\External declaration.
@End(Description)
The following two symbols can appear in the representations of declarations, but not in expressions:
@Begin(Description, fill 0, indent -1 in)
@lt<fcn+>@\Indicates a function declaration.
@lt<list+>@\Encloses a list (written with braces in the source) of initializer values.
@End(Description)
@End(Comment)
@Begin(Description, fill n, spread 0 lines, indent -.75 in, font small)@Comment{"fill 0" used to make blank lines
unnecessary, sigh...}

+@\Addition (one argument may be a pointer).

-@\Subtraction (one or both arguments may be pointers).

*@\With one argument, pointer dereferencing; with two, multiplication.

/ (//)@Foot{Slashified representations are given in parentheses when appropriate.}@\Division.

%@\Remainder.

<<@\Shift left.

>>@\Shift right.

&@\Bitwise AND.

| (/|)@\Bitwise OR.

^@\Bitwise XOR.

~@\Bitwise NOT (one argument).

@Blankspace(0.5 line)

==@\Equality comparison.

!=@\Inequality comparison.

<@\Less-than comparison.

>@\Greater-than comparison.

<=@\Less-than-or-equal comparison.

>=@\Greater-than-or-equal comparison.

!@\Logical NOT.

&&@\Logical AND.

|| (/|/|)@\Logical OR.

@Blankspace(0.5 line)

=@\Assignment.

++x@\Preincrement.

x++@\Postincrement.

--x@\Predecrement.

x--@\Postdecrement.

+=@\Add and assign (first argument may be a pointer).

-=@\Subtract and assign (first argument may be a pointer).

*=@\Multiply and assign.

/= (//=)@\Divide and assign.

%=@\Take remainder and assign.

<<=@\Shift left and assign.

>>=@\Shift right and assign.

&=@\Bitwise AND and assign.

|= (/|=)@\Bitwise OR and assign.

^=@\Bitwise XOR and assign.

@Blankspace(0.5 line)

[]@\Array reference.

. (/.)@\Structure element reference.

->@\Indirect structure element reference.

@Blankspace(0.5 line)

progn+@\The comma operator (which acts like the Lisp progn).

?: (?/:)@\The expression conditional.

@Blankspace(0.5 line)

if@\The statement conditional.

block+@\Encloses statements in a block (see text).

goto@\Go to the specified label.

label+@\The first argument is a label; the second is a statement.

while@\(while @i<looptest> @i<body>): iterate.

do@\(do @i<body> @i<looptest>): iterate, doing @i<body> at least once.

for@\(for @i<init> @i<looptest> @i<increment> @i<body>): iterate with syntactic sugar.

break@\Skip to the end of this while, do, for, or switch.

continue@\Skip to the next iteration of this while, do, or for.

return@\Return from this function (argument, if any, is value to return).

switch@\(switch @i<exp> @i<body>): go to the case in @i<body> matching @i<exp>.

case@\(case @i<value>): meaningful only in switch bodies.

@Blankspace(0.5 line)

cast+@\(cast+ @i<type> @i<expression>): cast the type of @i<expression> to @i<type>.

sizeof@\The size of a type or instance.

#lisp (/#lisp)@\Encloses a group of Lisp forms introduced with #lisp (see p. @Ref(LispInclPage)).

@Blankspace(0.5 line)

defunc+@\Function definition (see text for explanation).

decl+@\External declaration.

@Text{The following two symbols can appear in the representations of declarations, but not in expressions:}

fcn+@\Indicates a function declaration.

list+@\Encloses a list (written with braces in the source) of initializer values.

@End(Description)

@Caption{ZETA-C primitives.}@Tag(PrimitivesFigure = Figure)@~
@End(Figure)

A complete list of the ZETA-C primitives appears in Figure @Ref(PrimitivesFigure).  These are all symbols in package
@lt<C:>; they name macros which invoke the ZETA-C analysis and translation apparatus.  Some of the names end in plus
signs to avoid conflict with legal C identifiers (those which are alphabetic, but do not end in "+", are ZETA-C reserved
words).  

@Begin(Group)
The syntax of two of these primitive macros deserves examples.  First, let's look at function definition.  Here
is a sample C function:@Index(example of, function representation)
@Begin(Display)
@st<char *
foo(x, y)
     int x, y;
{
     int quux;
     bar(x + y);
     }>
@End(Display)
Here is its ZETA-C internal representation:
@Begin(Display)
@st<(DEFUNC+ ((CHAR) (* (FCN+ FOO X Y))) (((INT) X Y)) (BLOCK+ (((INT) QUUX)) (BAR (+ X Y))))>
@End(Display)
@Hinge
And here are some sample declarations:@Index(example of, declaration representation)
@Begin(Display)
@st<char foo(), *bar, *baz[47];
struct thing {>
	@st<int who, *why;>
	@st<struct thing *this, *that;>
	@st<} thingarray[128];>
@End(Display)
And here are their internal representations:
@Begin(Display)
@st<(DECL+ (CHAR) (FCN+ FOO) (* BAR) (* ([] BAZ 47)))
(DECL+ ((STRUCT THING ((INT) WHO (* WHY)) (((STRUCT THING)) (* THIS) (* THAT)))) ([] THINGARRAY 128.))>
@End(Display)
@End(Group)

Note some features of this representation:

@Itemize{
The name of the function is embedded inside a type declarator (see below).

The second subform of the @lt<defunc+> form is a list of parameter declarations.

The third subform is the body, and is always a @lt<block+> form.

The first subform of a @lt<block+> form is a list of local variable declarations.
}


@Paragraph(Representation of types)@Index(representation, type)

ZETA-C represents types internally as list structure.  Figure @Ref(TypesFigure) shows the type description language.

@Begin(Figure)
@Begin(Description, fill n, spread 0 lines, indent -1 inch, font small)
:char@\A character.

:signed-char@\A signed character.

:short@\A short (16-bit signed).

:unsigned-short@\An unsigned short.

:int@\Used for an int or long: an arbitrary-precision signed integer.

:unsigned@\Used for an unsigned int or unsigned long: an arbitrary-precision unsigned integer.

:float@\A single-precision floating point number.

:double@\A double-precision floating-point number.

:zero@\The constant 0.

:void@\The void type.

:lispval@\A lisp value (can be assigned or passed to and from functions, but nothing else).

:boolean@\The type of an expression evaluated for control, not value.

(:pointer @i<type>)@\A pointer to type @i<type>.

(:pointer @i<type> :null)@\A null pointer to type @i<type>.

(:array @i<type> @i<length>)@\An array of @i<length> elements, each of type @i<type>.  @i<length> can be NIL, meaning the length is not known.

(:function @i<type>)@\A function returning type @i<type>.

(:struct . @i<tag-or-elts>)@\A structure.  If @i<tag-or-elts> is a symbol, it's a tag; if a list, it's an alist associating element names
with types and accessing information.

(:union . @i<tag-or-elts>)@\A union; like :struct.

(:enum . @i<tag-or-elts>)@\An enumeration type; like :struct.
@End(Description)

@Caption{ZETA-C type descriptions.}@Tag(TypesFigure = Figure)


@End(Figure)


@Paragraph(Compilation error reference)@Index(errors, compilation)

@ErrorDoc(Message "@st<Assignments to structures are not allowed.  If you would like to permit them (for
UNIX> @st<compatibility), setq zeta-c:*firstclass-structures* to T.>@Index(struct, assignment)@Index(assignment of
struct)@Index(firstclass structures)@Index(@st<zeta-c:*firstclass-structures*>)@Index(@st<*firstclass-structures*>)", Doc
"See p.  @Ref(FirstclassStructuresPage).")

@ErrorDoc(Message "@st<Attempt to call @it<exp> of type @it<type> as a function>@Index(function call)", Doc "Chances are,
either you have a variable with the same name as a function, or you wrote some expression like @lt<(*ftab[ifunc])()>
incorrectly, or you declared something incorrectly.")

@ErrorDoc(Message "@st<Attempt to use expression @it<exp>, of type @it<type>, as a predicate>@Index(predicate)", Doc
"Arrays, structures, and unions may not be used as predicates in conditionals, as in, @i<e.g.>, @lt<if (frob)> ... where @lt<frob> is a
structure.")

@ErrorDoc(Message "@st<BREAK not inside WHILE, FOR, DO, or SWITCH>@Index(@st<break> statement)", Doc "A @lt<break>
statement may only appear lexically inside one of these constructs.")

@ErrorDoc(Message "@st<CASE not inside SWITCH>@Index(@st<case> statement)", Doc "A @lt<case> statement may only appear in the
body of a @lt<switch> statement.")

@ErrorDoc(Message "@st<CONTINUE not inside WHILE, FOR, or DO>@Index(@st<continue> statement)", Doc "A @lt<continue>
statement may only appear lexically inside one of these constructs.")

@ErrorDoc(Message "@st<Element @it<name> not found in struct/union type @it<type>>", Doc "You have attempted to reference a
structure element which is not present in this structure.  Remember that ZETA-C does not use a single namespace for
structure elements (see p. @Ref(StructNamespacePage)).")

@ErrorDoc(Message "@st<Excess initializer values: @it<vals>>@Index(initializer)", Doc "You have given a list of initializer
values which is longer than the aggregate being initialized.")

@ErrorDoc(Message "@st<Expression @it<exp> of type @it<type> cannot be used as an lvalue>@Index(lvalue)", Doc "You have
attempted to assign to, increment, or take the address of something which is not a variable or array or structure
element.")

@Comment{ This has changed -- to what???
@ErrorDoc(Message "@st<Function @it<name> called with wrong number or type(s) of arguments>", Doc "ZETA-C checks the
types of arguments in function calls at fasload time.")}

@ErrorDoc(Message "@st<Illegal use of storage class @it<sclass> in context @it<context>>@Index(storage class)", Doc "The
storage class of a declaration does not make sense in the context; for instance, @lt<auto> in an external declaration.")

@ErrorDoc(Message "@st<Initializer expression @it<exp> is not of type @it<type>>", Doc "The type of the initializer expression
supplied does not match the type, @it<type>, of the variable being initialized.")

@ErrorDoc(Message "@st<Initializer nested too deeply: @it<init-exp>>", Doc "A brace-delimited list of initializer values was
supplied where only a single, undelimited value was expected.")

@ErrorDoc(Message "@st<Internal error: @it<message>>", Doc "One of ZETA-C's internal consistency checks has been violated.
This represents a bug in ZETA-C.  If possible, please send a bug report with a full backtrace and a copy of the C code
that excited the bug.  To get into the error handler in the error context, you may have to set @lt<compiler:warn-on-errors>
to @lt<NIL> and recompile.")

@Comment{??? this has changed too
@ErrorDoc(Message "@st<Mismatched argument types to = (assignment): @it<type1> and @it<type2>>@Index(assignment)", Doc
"The type of the variable being assigned to does not match that of the value being assigned.  The only automatic
coercions are the numeric coercions and coercion of the constant zero to the null pointer.")}

@ErrorDoc(Message {@st<Mismatched consequent and alternate types to "?:": @it<type1> and @it<type2>>@Index(?: operator)},
Doc "In a conditional expression, the types of the two expressions on either side of the colon must match (one must be
coercible into the other).")

@ErrorDoc(Message "@st<Parameter @it<name> appeared more than once in the parameter list @it<list>>@Index(parameter list)",
Doc "The same name may not be used for two different parameters of a function.")

@ErrorDoc(Message "@st<Returning structures from functions is not allowed.>@Index(struct, returning)
@Index(firstclass structures)@Index(@st<zeta-c:*firstclass-structures*>)@Index(@st<*firstclass-structures*>)",
Doc "The treatment of structures as firstclass objects has been disabled.  If you would like to enable it -- to allow
structures to be assigned, passed as arguments, and returned from functions -- set @lt<zeta-c:*firstclass-structures*> to
@lt<T>.  (See p.  @Ref(FirstclassStructuresPage)).")

@Comment{??? ain't I going to fix this?}
@ErrorDoc(Message "@st<The body of a switch statement must be a block, with no declarations>@Index(@st<switch>
statement)", Doc "This is a ZETA-C restriction.  (Blocks @it<inside> the body may have declarations.)")

@ErrorDoc(Message "@st<Type mismatch between function declaration and RETURN value: @it<type1> declared, @it<type2>
returned>", Doc "The value you are attempting to return is not of the same type as the declared type of the function.")

@ErrorDoc(Message "@st<Undeclared struct/union tag: @it<tag>>", Doc "You have written @lt<struct @it<tag>> or @lt<union @it<tag>> without
declaring or having declared @it<tag>'s elements.")

@ErrorDoc(Message "@st<Variable @it<name> appeared in the parameter declarations, but is not one of the
parameters @it<list>>@Index(parameter list)", Doc "You have declared @it<name> in the parameter declarations at the head of a
function, but @it<name> is not in the parameter list of the function.")

@ErrorDoc(Message "@st<Wrong argument type >...", Doc "You have supplied a value of incorrect or nonsensical type to
one of the ZETA-C primitives; for example, perhaps you attempted to add a structure or multiply a pointer.")


@Subsection(Runtime debugging)@Index(debugging)

[[This subsection will list some runtime errors whose meaning in the context of a C program may not be clear, and
suggest places to look to find the real problem.]]

[[It will also talk about the use of the Lisp debugger for debugging C, or the C object debugger if that gets written.]]


@Section(Program construction)@Tag(ProgramConstructionPage = Page)@Index(program construction)

You have several options in building programs using ZETA-C.  The simplest approach is to keep files of C code, compile
and load them by hand, and call the functions in them by hand from a Lisp listener or from other Lisp or C programs.
Or, if you want your C programs to be callable from Lisp in roughly the same way that UNIX programs are callable from
the shell, you can use @lt<zeta-c:create-c-program> to create the appropriate top level.  You can use the Lisp
@lt<defsystem>@Index(@st<defsystem>) facility to partially automate the maintenance of these "stand-alone" programs, or to
integrate C code into Lisp programs (ZETA-C itself takes this approach -- its parser was built using the UNIX @b<lex> and
@b<yacc> utilities, which produce files of C code).

You should bear in mind, while preparing C programs for execution on the Lisp Machine, the way ZETA-C depends on
the Lisp package system@Index(package, C program) (see p. @Ref(PackagesPage)).  You will need to select a name for
your C program package, and add a file attribute list at the beginning of each file specifying this package and C mode
(see p.  @Ref(AttributeListPage))@Index(attribute list)@Index(@NoTCT{-*- line}).  If your program redefines any of the
functions in the ZETA-C standard library (see chapter @Ref(LibraryChapter)), you should arrange to shadow their
names in your C program package; one way to do this is to include at the beginning of the main header (@lt<.h>) file for the
program lines like

@Begin(Display)@Index(shadowing C library functions)
@st<#lisp
  (gl:shadow '(|putc| |getc|))
#endlisp>
@End(Display)

Alternatively, if you are using @lt<defsystem>, you can add the shadowing declaration to the package definition in the
system definition file.

Here are the tools available for compiling files and building programs:

@FunctionDoc(Name "@lt<@CCFile>@Index(@st<@CCFile>)@Case(Brand, Symbolics "@Index(@st<c-compile-file>)", TI
"@Index(@st<c-compile-file>)")", Arglist "@it<infile> &optional @it<outfile>", Doc "C-compiles the specified file, producing a
standard @Qfasl file.  All declarations and #definitions made by the file compiled or by any of the files it includes
remain in effect after the compilation is complete, and are accessible via the C listener.")

@Comment(Does anyone ever need @st<zeta-c:create-c-package>?  May be necessary in System 94 and Rel 4.  If so,
document here.)

@FunctionDoc(Name "@lt<zeta-c:create-c-program>", Arglist "@it<name>", Doc {Sets up a function called @it<name> -- in package
@lt<GLOBAL:> if possible, else in the @it<name@lt<:>> package -- which is to be used as the top-level invocation function for a C program.
The created function takes one required argument, which is the current directory in which the program is to be run,
followed by an &rest argument of strings, which are the "command line arguments".  It initializes externals, binds @lt<stdin>
etc. to the appropriate streams, packages up the "command line arguments" into @lt<argc> and @lt<argv> in the standard UNIX
way, and calls @it<name@lt<:main>>.  For instance,
@Display[@st<(zeta-c:create-c-program 'cweed)>]
sets up a function @lt<cweed> which might then be called thus:
@Display[@st<(cweed "oz:<x.gyro>" "-afd" "oz:<x.gyro>foo.text")>]})

@Hinge
The following transformations are provided for users of @lt<defsystem>.  (See the section on @lt<defsystem> in your Lisp Machine
documentation.)

@RandomDoc(Name "@lt<:c-compile> @it<input> @it<dependencies> @it<condition>", Kind "Defsystem transformation", Doc "Calls @lt<cc-file> to
compile the indicated files, whose names must have canonical type @lt<:c>.  @it<condition> defaults to
@lt<si:file-newer-than-file-p.>")

@RandomDoc(Name "@lt<:c-compile-load> @it<c-dep> @it<l-dep> @it<c-cond> @it<l-cond>", Kind "Defsystem transformation", Doc "Equivalent to
@lt<(:fasload (:c-compile @it<c-dep> @it<c-cond>) @it<l-dep> @it<l-cond>)>.")


@Section(C Listener)@Index(C listener)

ZETA-C provides a "C listener", a counterpart to the Lisp Listener, for the manual entry and execution of C expressions
and declarations.  You can get to an independent C listener (that runs in its own process) by typing @st<@SystemKey {>, by
selecting the C Listener item in the Programs column of the System Menu, or by the Create, Split Screen, or Edit Screen
menus in the Windows column of the Lisp Listener.  Or, you can get a C listener that runs in your Zmacs process by
typing @st<@BreakKey> to a Zmacs window whose buffer is in C mode (hit @st<@BreakKey> again to get a Lisp listener).

(The C listener is on @st<@SystemKey {> rather than @st<@SystemKey C> because the latter is standardly used for the
Converse program, and @st<{> is a character suggestive of the C language.  However, your local system maintainer may have
put Converse on some other key, making @st<C> available for the C listener.  Type @st<@SystemKey [Help]> to be sure.)

When you create an independent C listener, the first thing it does is ask you what package you would like to work in.  If
this package already exists, it must be a C program package (see p. @Ref(PackagesPage)); if it is not, the C listener will
request another package name.  If the package does not exist, the C listener offers to create it.  Next, the C listener will
ask you if you wish the external and static variables in this package to be freshly initialized; you may want this if you
are debugging a program and want to start its execution over from the beginning.  Then the listener will request a default
directory for file operations to take place in.  Finally, the listener will issue a prompt (usually @st<C:>, but see
below) and await your typein.  If at any time you wish to change the package or directory, or to reinitialize externals and
statics, type @st<meta-[Abort]>, and this series of questions will be repeated.

At this point -- whether you've created an independent C listener or one within Zmacs -- you may type any of the following:
a statement, a declaration, a function definition, or even a preprocessor directive.  A statement may consist merely of an
expression followed by a semicolon; the expression will be evaluated and its value printed.  (Thus a C listener makes an
excellent infix-notation desk calculator!)  Just as with a Lisp listener, parsing happens on the fly, and the statement,
declaration, or definition will be processed as soon as it is syntactically complete (as soon as you type the closing
semicolon or right brace); a preprocessor directive will require a @st<[Return]>.  Also, the @RuboutHandlerTerm is fully
available for editing input, along with the history feature for reentering previous inputs.

@Begin(Group)
Here are some examples; user typein is in @lt<bold>.

@Begin(Display)@st< 
C package to work in? @lt<ctest>
Create C package CTEST? (Y or N) @lt<Y>es.
Initialize externals and statics in package CTEST? (Y or N) @lt<N>o.
Working directory for file I/O? astarte:gyro;
C: @lt<int foo;>>				@st<@small{foo is declared}
C: @lt<foo;>>					@st<@small{foo's value is requested}
(int) 0>						@st<@small{it's zero, of type int}
C: @lt<char *bar = "A string";>>		@st<@small{bar is declared and initialized}
C: @lt<bar;>>					@st<@small{bar's value is requested}
(char *) ">->@st<A string">			@st<@small{the arrow indicates a pointer to that character of the string}
C: @lt<for (foo=0; foo<5; ++foo) printf("%s\n", bar + foo);>
A string>						@st<@small{a "for" loop with a function call}
 string>						@st<@small{does just what you'd expect}
string
tring
ring
C: @lt<#include <stdio.h>>>			@st<@small{we want to do some fancier I/O}
C: @lt<viewfile (name)>>			@st<@small{we type in a simple function} 
     @lt<char *name;>>			@st<@small{(actually, this is probably more complex}
@lt<{>>							@st<@small{a function than you would actually use the} 
     @lt<FILE *fd;>>				@st<@small{C listener for, but it's a good example)} 
     @lt<int c;> 
     @lt<if ((fd = fopen(name, "r")) == NULL) {> 
          @lt<printf("Can't open '%s'.\n", name);> 
          @lt<exit(0);> 
     @lt<}> 
     @lt<while ((c == getc(fd) != EOF) putchar(c);> 
     @lt<fclose(fd);
}>>							@st<@small{we type the closing brace, and the function gets compiled}
C: @lt<viewfile("hello.c");>>			@st<@small{and we run it}
/* -*- Mode: C; Package: (hello C) -*- */>		@st<@small{printing the file we typed in long ago}
main() {
     printf("\nHello, world!\n");
}
C: @lt<#include "hello.c">>		@st<@small{we load that file}
C: @lt<HELLO$main();>>			@st<@small{and call its function (note the package specification)}
Hello, world!
C:>							@small{etc....}
@End(Display)@Hinge@End(Group)

Currently (Release @Value(Release)), errors in execution of C expressions will still invoke the standard Lisp Debugger. 
Also, typing @st<@BreakKey> will give you a Lisp read-eval-print loop.

@VariableDoc(Name "@lt<zeta-c:*c-listener-prompt*>", Doc "A string containing the prompt issued by a C listener when
it is ready for an expression.  The default value is @st<"C:@_">.")


@Section(Function Type Checking)@Index(function type checking)@Index(type checking of functions)
@Index(argument type checking)@Index(@w<return value, function>)@Tag(IncrLintPage = Page)

Along with incremental compilation, ZETA-C provides an "incremental @b<lint>" facility that checks, at @i<load> time, that the
types of function arguments match the types of the corresponding formal parameters, even when the call and the
definition are in different files.  "Load" time, in this case, is either when the @Qfasl file is loaded, or, for code being
compiled incrementally, essentially the same as compile time.  That is to say, when a function is compiled incrementally
or loaded from a file, all known calls to it are checked as well as all the calls it makes to other known functions.  The
warnings generated in case of mismatches are placed in the compiler warnings database, and can be reviewed via the
appropriate Zmacs commands (see the section on compiler warnings in your Zmacs manual).

Similar checking is performed for the type of the return value of a function.

@Comment{Also, did I put in incremental-lint for external variables?  If so, rename section & check references to
IncrLintPage.}


@Chapter(Editing C with Zmacs)

ZETA-C provides some Zmacs extensions to simplify the editing of C programs.  These extensions fall into several
categories: cursor movement; indentation support and other convenience commands; sectionization; and compilation.  In
order to activate these, you must specify C mode in the file attribute list ("-*- line") of each C source file; for instance

@Display{@Tag(AttributeListPage = Page)@Index(attribute list)@Index(@NoTCT{-*- line})
@st</* -*- Mode: C; Package: (@it<name> C) -*- */>
}

Note that the attribute list must be enclosed in comment delimiters (@st</*> ... @st<*/>).


@Section(Cursor movement)@Index(cursor movement in Zmacs)

@KeyDoc(Key "@lt<control-meta-A>", Name "Beginning of C Function or Declaration@Index(Beginning of C Function...)",
Doc "Moves to the beginning of the current C function or declaration.  If the cursor is already at the beginning of one,
moves to the beginning of the previous one.")

@KeyDoc(Key "@lt<control-meta-E>", Name "End of C Function or Declaration@Index(End of C Function...)", Doc "Moves to
the end of the current C function or declaration.  If the cursor is already at the end of one, moves to the end of the
next one.")

@KeyDoc(Key "@lt<control-meta-H>", Name "Mark C Function or Declaration@Index(Mark C Function...)", Doc "Puts @b<point>
at the beginning, and @b<mark> at the end, of the current C function or declaration.  Given a positive or negative numeric
argument, marks that many objects forward or backward, respectively.")


@Section(Indentation and misc.)@Index(indentation of C code)

C programs are indented in a variety of different styles.  ZETA-C's indentation support assumes that braces are written
only at the ends of lines; within this constraint, the following variations are supported:

@VariableDoc(Name "@lt<zwei:*C-block-indentation*>@Index(@st<zwei:*C-block-indentation*>)@Index(@st<*C-block-indentation*>)",
Doc "The distance, in spaces, to indent nested C blocks.  If @lt<NIL> (the default), the distance defaults to the current tab
width.")

@VariableDoc(Name "@lt<zwei:*C-indent-}-as-outside*>@Index(@st<zwei:*C-indent-}-as-outside*>)
@Index(@st<*C-indent-}-as-outside*>)", Doc "If @lt<T> (the default), a right brace on a line by itself is lined up with the
statements @i<outside> the block it closes:
@Display[@st< 
@;        foo();
@;   }
@;   bar();>]
If @lt<NIL>, the brace is lined up with the statements @i<inside> the block:
@Display[@st< 
@;        foo();
@;        }
@;   bar();>]
")@Comment{Those @;s are needed because we're inside a string (yeesh).}

@KeyDoc(Key "@lt<Tab>", Name "Indent for C@Index(Indent for C)", Doc "Indents the current line in the current C style.
With a numeric argument, indents that many lines starting at the current line.")

@KeyDoc(Key "@lt<control-meta-Q>", Name "Indent Region for C@Index(Indent Region for C)", Doc "Indents all lines in the
region in the current C style.")

@KeyDoc(Key "@lt<control-;>", Name "Indent for Comment@Index(Indent for Comment)@Index(comment, indentation)", Doc
"Moves to or creates a comment.  Finds the start of any existing comments, or creates one at the end of the current line.
With a numeric argument, re-aligns existing comments for that many lines, but does not create any.  @lt<*COMMENT-COLUMN*>
is the minimum column (in pixels) for aligning comments.  (This is, in fact, the standard Indent for Comment function;
C mode just sets up the string variables specifying the comment-start and -end strings.)")

@KeyDoc(Key "@lt<meta-;>", Name "End Comment@Index(End Comment)", Doc "Closes the comment on this line (if any) and
moves to the next line.")

@KeyDoc(Key "@lt<meta-P>", Name "Up Comment Line@Index(Up Comment Line)", Doc "Moves to or creates a comment on the
previous line; but first, if the current line contains a null comment, it is deleted.")

@KeyDoc(Key "@lt<meta-N>", Name "Down Comment Line@Index(Down Comment Line)", Doc "Moves to or creates a comment
on the next line; but first, if the current line contains a null comment, it is deleted.")

@KeyDoc(Key "@lt<control-meta-R>", Name "Reposition Window for C@Index(Reposition Window for C)", Doc "Tries to get
all of the current C function or declaration on the screen.  Repeated invocations will scroll the comments immediately
above the function or declaration on and off the screen, alternately.")


@Section(Sectionization)@Index(sectionization)@Index(declarations, in editor)@Index(@w<function, in editor>)

The term "sectionization" is Zmacs jargon for finding the boundaries between the top-level declarations and function
definitions that appear in a source file.  The sectionizer is also responsible for finding a suitable name for each section;
the name it chooses will be used by @st<meta-.> and other section-related commands.  Since the indentation and layout
conventions for C are considerably less standardized than they are for Lisp, and since C syntax is more complex, it is a
lot harder to tell where sections begin and end in C programs.  In order that the sectionizer should still work reasonably
on syntactically incorrect code, it does not even attempt to parse a file completely.  Instead, it uses a simple heuristic to
guess where things must begin and end.@Foot{The Lisp sectionizer, as you may know, takes the same approach -- it
just looks for left parentheses in column 0.  The heuristic for C is more complex but in the same spirit.} Basically,
whenever it sees a word -- as opposed to punctuation -- starting in column 0, it considers that line to start a section.  If
the section contains a function definition, the section takes its name from the function, just as you would expect; if the
section contains a declaration, the section takes its name from the first identifier declared (in the case of a structure or
union declaration, this is the structure or union tag if one is given, otherwise the name of the first element).

To repeat: any word in column 0 is taken to start a section.  It is worth mentioning some ramifications of our use
of this heuristic.

@Begin(Itemize)
Each line of a comment must start with whitespace or some non-alphabetic character.@Index(comment, problems with
sectionizer)

Each line of a function but the first must be indented, unless it starts with punctuation such as "{"; we have seen C
indentation styles wherein the declarations of functional parameters were not indented, thus:
@Display[
@st<foo(a)
int a;
 { >...]
The parameter declaration must be indented:@Index(parameter declarations, problems with sectionizer)
@Display[
@st<foo(a)
     int a;
{ >...]

Likewise, statement labels must be indented.@Index(labels, problems with sectionizer)

If an external variable declaration is to be recognized as starting a section, it must begin in column 0.  Alternatively, if
you wish to group several consecutive declarations into one section (so all of them can be recompiled if any has
changed), indent all but the first.@Index(declarations, problems with sectionizer)

Preprocessor directives never start sections.  Furthermore, each section extends to the beginning of the next section, so
preprocessor directives are considered to belong with the @i<preceding> declaration.@Index(preprocessor directives, problems
with sectionizer)
@End(Itemize)

@i<EXCEPTION>: we specifically provide for the type of a function being defined to go on a line by itself, thus:
@Display[
@st<struct env *
parent(e)
     struct env *e;
{ >...]

If you are uncertain about exactly how some code was sectionized, you can do @lt<meta-X List >@Case(Class, Symbolics
"@lt<Definitions>", LMITI "@lt<Sections>") to see if any of the sections are spurious or missing.  If you are wondering why
anyone would care, read on to the next section!


@Section(Compilation)@Index(compilation, in editor)@Index(incremental compilation)

ZETA-C provides commands for compilation of single sections (see above), specified regions, or entire buffers.

@KeyDoc(Key "@lt<hyper-control-C> (or @lt<control-shift-C>)", Name "@Case(Class, LMITI "C ")Compile Region", Doc "If there
is a region, it is C-compiled; otherwise, the current section (function definition or external declaration) is C-compiled.")

@KeyDoc(Key "@lt<hyper-meta-C> (or @lt<meta-shift-C>)", Name "@Case(Class, LMITI "C Compile Buffer Changed Sections",
Symbolics "Compile Buffer Changed Definitions")", Doc "C-compiles any sections in the current buffer that have been
edited since they were last compiled.  With a numeric argument, asks whether to compile each changed section.")

@ExtComDoc(Name "@lt<meta-X @Case(Class, LMIT "C ")Compile Buffer>", Doc "C-compiles the entire current buffer.
With a numeric argument, compiles the rest of the buffer (from @b<point> to the end).")

ZETA-C provides an "incremental @b<lint>" facility to keep track of function argument and return value types and verify
consistency across files.  Incremental compilation is one way to invoke this facility.  See p. @Ref(IncrLintPage) for more
information.

@Comment{Is there?
Also, note that there is no automatic mechanism for keeping track of dependencies between declarations and code.  If
you change the type of a variable, it is your responsibility to find and recompile all the code that references that
variable.@Index(dependencies of code on declarations)}



@Chapter(The ZETA-C Dialect)@Tag(IncompatibilitiesChapter = chapter)@Index(incompatibilities)

Our goal has been and continues to be to make ZETA-C highly compatible with standard C implementations, most
notably the Berkeley 4.2bsd and Bell UNIX(R) System V compilers, as well as the specification given in @i<C: A Reference
Manual> by Samuel P. Harbison@Index(@w<Harbison, Samuel P.>) and Guy L. Steele Jr.@Index(@w<Steele, Guy L. Jr.>)
(Prentice-Hall, 1984).  (We also plan to meet the ANSI C standard, as soon as that standard is released to the public.)
This chapter documents in detail remaining incompatibilities, known bugs, and implementation-defined behavior.  We
start with a couple of sections describing extensions ZETA-C makes to standard C.

@Begin(Comment)
For Yacc:
 -- file attribute list
 -- file name (includes and otherwise), of course, depending on the file system
 -- column 0 and sectionization
 -- can't cast pointers to ints before comparing them (only needed when grossly reusing memory, as above)
 -- @b<optarg> and @b<restarg>, of course
@End(Comment)


@Section(Variable numbers of arguments)@Tag(varargsPage = Page)@Index(variable numbers of arguments)
@Index{optarg (parameter declaration)}@Index{restarg (parameter declaration)}

In traditional C implementations, there is no checking at either compile or run time that the number of arguments
passed to a function is the same as the number of parameters the function declares.  In fact, most existing C programs
are written assuming that it is possible for functions to take a variable number of arguments; either the last one or more
arguments will simply be omitted from a call, or functions (notably @lt<printf>) will accept what amounts to a
variable-length list of arguments.

The Lisp Machine, on the other hand, provides run-time checking (and compile-time checking too, for calls to known
functions) of the number of arguments passed to a function, and requires explicit declarations (@lt<&optional> and @lt<&rest>
keywords in Lisp) when the function is to take a variable number of arguments.  ZETA-C likewise requires explicit
declarations of optional and "rest" arguments, by means of the storage class specifiers @lt<optarg> and @lt<restarg> (applicable
only to parameters).  @lt<optarg> indicates that the argument (actual parameter) may be omitted; all subsequent arguments
may also be omitted, even if the corresponding parameters were not specifically declared @lt<optarg>s.  @lt<restarg>, on the other
hand, indicates that an arbitrary number of arguments may be passed corresponding to this parameter; it is ignored
except on the last parameter in the list.  For example:

@Begin(Display)
@st<foo(a, b, c)
     int a;
     optarg int b;
     char *c;
{>
     @it<[... some stuff ...]
@st<}>>

@st</* These calls to foo will all work */
     foo(1);
     foo(2, 476);
     foo(3, 476, "Hello");
/* These will get a wrong-number-of-arguments error */
     foo();
     foo(44, 55, "Hello", "Goodbye");>

@st<send_multiple(dest, messages)
     frob *dest;
     restarg char *messages;
{
     /* We send each message to the destination */
     /* The end of the argument list is indicated by NULL */
     for (i = 0; (&messages)[i]; ++i)
           send(dest, (&messages)[i]);
}
/* An example call to send_multiple */
     send_multiple(turtle, "Forward 4", "Right 45",
                   "Forward 96", NULL);>
@End(Display)

The technique shown in @lt<send_multiple> for accessing each of the supplied arguments is not strictly portable, since it
assumes that successive arguments are stored at successively higher addresses, but all the compilers we are familiar with
work this way, and this trick has become quite common in C.  Note that it is not the parameter itself, but rather its
@i<address>, that is the base of the array of arguments.  Note also that you must provide your own convention for
determining the end of the argument list: under ZETA-C, an array-bounds error will be signalled if you attempt to
access a nonexistent element of the rest-argument array, but under traditional compilers, you will just get garbage.

@b<RESTRICTION>: @i<do not> modify any of the rest-arguments if these arguments are structures.  ZETA-C does not make
copies of these arguments, as the semantics of C requires.  (C is defined to be call-by-value, and ZETA-C conforms to
this definition in all but this particular case, where its behavior is call-by-reference.)

To compile your program with a traditional compiler, include the lines
@Display{
@st<#define optarg
#define restarg>
}
(other changes may also be necessary, as there is no completely portable way to write functions that accept variable
numbers of arguments).


@Section(Lisp objects in C programs)@Tag(LispvalPage = Page)@Index(Lisp objects in C)@Index(lispval (type))

ZETA-C provides a type, @lt<lispval>, which is intended for variables holding miscellaneous Lisp objects (e.g., lists, flavor
instances).  The operations defined on @lt<lispval>s are: assign, pass as function argument, return as function value,
compare for equality (the comparison is done with @lt<eql>), or test as conditional predicate with @lt<if>, @lt<?:>, @lt<!>, @lt<&&>, or @lt<||> (for
this purpose, @lt<nil> is treated as @b<false> and non-@lt<nil> as @b<true>, just as in Lisp).


@Section(Inclusion of Lisp code)@Tag(LispInclPage = Page)@Index(Lisp code in C function)@Index(Inclusion of Lisp code)

ZETA-C supports two forms of inclusion of Lisp code.  One permits entry of a Lisp form as an @i<expression> in C, the other
includes one or more Lisp forms as C @i<statements.>  The expression form (useful primarily in the C listener, but also
available in C source files) is @lt<@@@i<form>>.  For instance, the demo program supplied with ZETA-C,
@st<zeta@;-c:source;turtle.c>, contains a line like:
@Display{@st<window = @@tv:(make-window 'window :edges-from :mouse);>}
Note that after the closing right parenthesis of the Lisp form is encountered, the syntax reverts to C, so that a semicolon
is required to complete the statement.  An expression entered with `@lt<@@>' has type @lt<lispval> (see above), but can of course be
cast to any type.  If it is cast to a pointer type, the expression is expected to return two values, which will become the
array part and index part, respectively, of the pointer (see the section on Pointers, page @Ref(PointersPage)).

Much as some C compilers provide the @lt<#asm> construct for literal inclusion of assembler code, ZETA-C provides for the
inclusion of Lisp forms as C statements via @lt<#lisp> ... @lt<#endlisp>@Index(#lisp directive).  The lines between these
directives are read as Lisp forms and included in the source, as handed to the Lisp compiler, without further processing.
If this construct is used inside a function definition, all arguments, locals, and statics of the function can be referenced
by name (see the section on Pointers, page @Ref(PointersPage), on the naming of pointer variables).  A @lt<restarg>
parameter will be bound to an array of the arguments.  Note that the Lisp forms will be read in the same package as the
C code; if you want to access another package, use an explicit prefix.  Also note that the vertical bar convention must
be used to reference variables whose names contain lowercase letters (see the next section).

Comments between @lt<#lisp> and @lt<#endlisp> must be introduced with semicolons!

Here is an example:
@Begin(Display)
@st<print_num (n)
     int n;
{
     /* For no good reason, we use FORMAT instead
      * of PRINTF to print the number. */
#lisp
     (gl:format gl:t "The number is: ~D" |n|)         ; Note "gl:"s
#endlisp
}>
@End(Display)


@Section(ZETA-C Identifiers)@Tag(IdentifiersPage = Page)@Index(identifiers)@Index(case of identifiers)

Unlike Lisp, C distinguishes upper and lower case in identifiers.  So C variable and function names are not
automatically converted to upper case.  This means that, when referencing a C variable or function from Lisp, one must
use Lisp's vertical bar notation if the name contains lowercase letters; for instance: @st<|main|>, or, if the reference is from
another package, "@st<hello:|main|>" (@i<not> "@st<|hello:main|>").  Conversely, when referencing Lisp functions from C, one must
write in uppercase, @i<e.g.>, "@st<TV$BEEP>".

ZETA-C uses the `@lt<$>' character (otherwise unused in C) for two purposes.  One is to delimit package prefixes, just as `@lt<:>'
does in Lisp.  So, for instance, the C statement "@st<TV$BEEP();>", equivalent to the Lisp "@st<(tv:beep)>", would beep the
console beeper (and/or flash the screen).  The other use of `@lt<$>' is to build internal names (called static alternate names)
of variables and functions declared @lt<static> (see the next section).  Here's how ZETA-C distinguishes these two uses: if
what appears before the first `@lt<$>' in a name is a known package name, then it is taken to be a package prefix; otherwise,
it is simply considered to be part of the name.  Package prefixes need not be written in upper case, though we
recommend this as a convention.

ZETA-C, like the Lisp in which it is implemented, supports arbitrarily long identifiers (well, to be honest, the parser,
being written in C, imposes a 4096 character limit), and all characters are significant.


@Section(Static variables and functions)@Index(static variables and functions)@Index(static alternate names)

One of the uses of the `@lt<$>' character (see the previous section) is to build internal names (called static alternate names)
of variables and functions declared @lt<static>.  This works in one of two ways:

@Begin(Itemize)
If a variable or function is declared @lt<static> at top level within a file -- @i<i.e.>, the declaration does not appear within a
function definition -- then its static alternate name is the name part of the file's pathname, followed by a `@lt<$>', followed
by the name of the variable or function as written.  For instance, if a file "@st<foo.c>" contains a declaration "@st<static int
bar;>", the alternate name for "@st<bar>" would be "@st<foo$bar>".

If a variable is declared @lt<static> within a function definition, then the static alternate name is the name of the
function, `@lt<$>', the name of the variable, `@lt<$>', a decimal number.  The decimal number is used to distinguish multiple static
variables of the same name in the same function; it is almost always "@st<1>".  So, the static alternate name of a variable
declared "@st<static int quux;>" within a function "@st<zot>", assuming there's only one such variable "@st<quux>", would be
"@st<zot$quux$1>".
@End(Itemize)

These two uses can compose, so if there's a variable declared "@st<static int quux;>" inside a function declared "@st<static
int zot() >..." in a file "@st<foo.c>", its static alternate name would be "@st<foo$zot$quux$1>".

A static variable or function can be referenced by its short name in the file or function in which it is declared, @i<after>
it has been declared static.  This means that, in the case of file statics, the variable or function must be declared static
textually @i<before> any reference to it appears.  (This is an incompatibility with standard C).  However, in the case of a
function, this does not mean that the entire function @i<definition> need appear near the beginning of the file; only that a
@i<declaration> of it as static be near the beginning.  So, for instance, one can say "@st<static int zot();>" near the beginning
of the file, refer to @lt<zot> throughout, and at the end define it: "@st<static int zot() { >...".

A static variable or function can be referenced by its alternate name from anywhere; specifically, from the C listener.
This is true even for static variables within functions (which was the purpose of all this, by the way: to make these
variables accessible from the C listener).


@Section(Dialect reference)

This section is written specifically as a companion to @i<C: A Reference> @i<Manual> by Samuel P. Harbison and Guy L. Steele Jr.
(Prentice-Hall, 1984) (abbreviated below "@i<C:ARM>").  For every section in that manual from which ZETA-C differs, or
where the behavior of C is specified as implementation-defined, we have included a correspondingly-numbered section
here.  We assume the reader has copy of @i<C:ARM> to refer to.


@ReSection(@Big(1  Introduction to C))

@ReSection(1.2  AN OVERVIEW OF C PROGRAMMING)

See the Overview and Examples chapter on p. @Ref(HelloPage) of this manual.


@ReSection(@Big(2  Lexical Elements))

@ReSection(2.1  THE SOURCE CHARACTER SET)@Index(character set, source)

ZETA-C uses the characters  `@lt<$>' and `@lt<@@>'; `@lt<$>' as the package prefix or static context delimiter, and `@lt<@@>' to introduce a
Lisp form (see the previous sections for descriptions of both of these).

@ReSection(2.1.1  Whitespace and Line Termination)@Index(line length)

There is no line length limit in ZETA-C.

@ReSection(2.1.2  Character Encodings)@Index(character set)@Index(\ escape sequences)

The Lisp Machine's character set is unusual in that the first 128 characters (0 through 177 octal) are all printing
characters; the control characters are octal 200 through 237.  See your Lisp manual for more details.  As long as you use
the predefined escape sequences @lt<\r>, @lt<\n>, @lt<\b>, @lt<\t>, @lt<\f>, and @lt<\v>, this will not make any difference to your programs, unless
you are using the high-order bits of characters for some special purpose.  The sequences @lt<\r> and @lt<\n> both name the
@st<[Return]> character, octal 215; @lt<\b> is @st<[Overstrike]>, octal 208; @lt<\t> is @st<[Tab]>, octal 209; and @lt<\f> and @lt<\v> are both @st<[Page]>,
octal 214.

@Begin(Group)
@ReSection(2.2  COMMENTS)@Index(comments)

Comments are normally treated as not nestable.  However, if you put in the file attribute list ("-*- line") of a C source
file the attribute "@st<Comments-nest: T>", then within that file (or any files it includes), comments will be treated as
nestable.
@End(Group)

@ReSection(2.4  OPERATORS AND SEPARATORS)

Compound assignment operators must be written as single tokens in ZETA-C; @i<e.g.>, "@lt<+=>", not "@lt<@w<+ =>>".

@ReSection(2.5  IDENTIFIERS)@Index(identifiers)

ZETA-C places a 4096 character limit (@i<i.e.>, effectively infinite) on identifier names; all characters are significant.  `@lt<$>' is
used as the package prefix delimiter (like `@lt<:>' in Lisp); see page @Ref(IdentifiersPage) above.

@ReSection(2.6  RESERVED WORDS)@Index(reserved words)

ZETA-C defines the additional reserved words @lt<optarg> and @lt<restarg> for declaring variable-length function argument
lists (see p. @Ref(VarargsPage)); @lt<signed> for declaring signed character variables (see p. @Ref(NumbersPage)); @lt<lispval>
for declaring variables to contain arbitrary Lisp objects (see p. @Ref(LispvalPage)); and @lt<packed_struct> for declaring
structures to be implemented with a "packed" representation (see p. @Ref(AggregatesPage)).

@ReSection(2.7  CONSTANTS)

@ReSection(2.7.1  Integer Constants)@Index(integer constants)@Index(constants, integer)

ZETA-C recognizes both the suffix '@lt<l>' or '@lt<L>', indicating a @lt<long> constant, and the suffix '@lt<u>' or '@lt<U>', indicating an @lt<unsigned>
constant; these may be used together.  Since ZETA-C supports arbitrary-precision integers, the value written will be the
value used (no "surprises" are possible like the ones listed in this section of @i<C:ARM>).

@ReSection(2.7.2  Floating-point Constants)@Index(floating-point constants)@Index(constants, floating-point)

Unless specially marked, a floating-point constant has type @lt<double>.  The suffix '@lt<f>' or '@lt<F>' indicates that the constant is
to be of type @lt<float>.  The suffix '@lt<l>' or '@lt<L>' is ignored (the type @lt<long double> is the same as @lt<double>).

@ReSection(2.7.3  Character Constants)@Index(character constants)@Index(constants, character)

Character constants have type @lt<char>, rather than @lt<int> as specified in this section of @i<C:ARM>.  (Since @lt<char> is an unsigned
type, there is only one situation in which this will make a difference.  Consider the comparison @w"@st<('A' > -1)>".  One
might expect this comparison to yield @b<false>, but since @lt<char> is unsigned, it will instead yield @b<true>, as in the example on
p. 90 of @i<C:ARM>.  Note that @w"@st<(c > -1)>" where @st<c> is a @lt<char> variable will also yield @b<true>, so by treating character
@i<constants> as @lt<char> rather than @lt<int>, ZETA-C brings their behavior into line with that of @lt<char> @i<variables>.)

Multicharacter constants are accepted, and treated as they are by the 4.2bsd compiler for the VAX: the rightmost
character in the constant goes in the low-order byte of the word (so a @w"@lt<char *>" pointer to this word would see it
@i<first>; this is "backwards" with respect to the way strings are stored).  2-character constants have type @lt<unsigned short>;
3- and 4-character constants have type @lt<unsigned>; longer constants have type @lt<unsigned long> (but note that their use
is @i<completely> nonportable).

@ReSection(2.7.4  String Constants)@Index(string constants)@Index(constants, string)

Identically written string constants within a single function will be represented by the same block of storage.  (This
optimization is performed by the Lisp compiler, and so we cannot disable it.)

@ReSection(2.7.5  Escape Characters)@Index(\ escape sequences)

Hexadecimal escape codes are also supported, in the form @lt<\x@it<dd>>, where @it<d> is a hexadecimal digit (@lt<0>-@lt<9>, @lt<a>-@lt<f>, @lt<A>-@lt<F>).


@ReSection(@Big(3  The C Preprocessor))

@ReSection(3.2  PREPROCESSOR LEXICAL CONVENTIONS)@Index(preprocessor)

The # must be the first character on the line; there may be any amount of whitespace between the # and the
preprocessor command.  It is not an error if non-whitespace appears on a line after a command that takes no arguments;
such text is ignored.

@ReSection(3.3.2  Defining Macros with Parameters)

In the @i<invocation> of a macro with parameters, ZETA-C will allow one space between the name of the macro and the left
parenthesis that begins the argument list, but will issue a portability warning whenever such a space is encountered.

ZETA-C does not recognize formal parameter names within string and character constants.

@ReSection(3.3.3  Rescanning of Macro Expressions)

Recursion in macro expansions is not detected, and causes ZETA-C to enter an infinite loop, which can be stopped by
typing @st<control-[Abort]>.  (But the example on the bottom of p.35 doesn't loop!  It gets a syntax error as soon as two
copies of the argument have been generated.  This happens because rescanning is interleaved with parsing.)

@ReSection(3.3.4  Predefined Macros)

ZETA-C's version of @st<stdio.h> contains the line
@Display{@st<#define ZETA_C>}

@ReSection(3.3.5  Undefining and Redefining Macros)

Macros may be redefined freely; the old definition is simply discarded.  No warning is issued.  ZETA-C does not keep a
stack of definitions; @lt<#undef> will ensure that no definition remains.

@ReSection(3.3.6  Some Pitfalls to Avoid)

Macro expansions are actually rescanned as character sequences, but since all sequences of whitespace and/or
comments turn into single spaces, this does not generally make any difference.  The exception is that the null comment
@st</**/> is explicitly recognized as a "token concatenation" operator.  Some examples (cf. p. 39 of @i<C:ARM>):
@Begin(Display)
@st<#define INC ++
#define TAB internal_table
#define INCTAB table_of_increments
#define CONC(x,y) x/**/y
#define CONC2(x,y) x /**/ y
#define DONTCONC(x,y) x/* */y
CONC(INC,TAB)  >=>@st<  INCTAB  >=>@st<  table_of_increments
CONC2(INC,TAB)  >=>@st<  INCTAB  >=>@st<  table_of_increments
DONTCONC(INC,TAB)  >=>@st<  INC TAB  >=>@st<  ++ internal_table>
@End(Display)
Note that the null comment functions as a concatenation operator regardless of whether there is whitespace around it in
the macro definition, but if there are any characters in the comment at all, it will not cause concatenation.

Macro text is checked for balanced single and double quotes when it is first encountered, so the example on the top of p.
40 of Harbison & Steele will cause an error.

@ReSection(3.5  CONDITIONAL COMPILATION)

@ReSection(3.5.5  The @lt<defined> Operator)

The @lt<defined> operator is not yet implemented.

@ReSection(3.6  EXPLICIT LINE NUMBERING)

@lt<#line> directives are ignored.  `@lt<#>' on a line by itself is ignored.  The use of `@lt<#>' as a synonym for `@lt<#line>' is not supported.


@ReSection(@Big(4  Declarations))

@ReSection(4.2  TERMINOLOGY)

@ReSection(4.2.1  Scope)@Index(scope of declarations)@Index(labels)@Index(statement labels)

The scope of a statement label is the innermost enclosing block that contains declarations of automatic variables, rather
than the entire function as the standard specifies.  The Lisp compiler enforces this very reasonable restriction (jumping
into a block that contains automatic variables is likely to cause unpredictable results anyway).

@ReSection(4.2.4  Overloading of Names)@Index(component namespaces)@Index(struct and union components)

Labels are not in the same name space as variables; but see @Sec@:4.2.1 above.  ZETA-C provides a separate component
namespace for each structure and union type (the "modern" interpretation).  Structure and union tags are in the same
name space, but enumeration tags are in a separate one.

@ReSection(4.2.8  Initial Values)@Index(initializers)

Like the UNIX compilers, ZETA-C initializes objects of static extent to zeros.  New code should, however, not depend on
this.  Since blocks with automatic variables cannot be entered abnormally in ZETA-C (see @Sec@:4.2.1 above), there is
no danger that the variables will not be properly initialized.

@ReSection(4.2.9  External Names)

ZETA-C will issue an "Undeclared variable" error for this example.

@ReSection(4.3  STORAGE CLASS SPECIFIERS)@Index(optarg)@Index(restarg)

ZETA-C defines the additional keywords @lt<optarg> and @lt<restarg>, which behave syntactically like storage-class specifiers
and which apply only to parameter declarations.  (See page @Ref(varargsPage).)

@ReSection(4.6  INITIALIZERS)@Index(initializers)

Many C compilers (including the UNIX Portable C Compiler, PCC) make some attempt to "do what you mean" when given
an initializer that does not quite conform to the rules.  ZETA-C, on the other hand, imposes a stricter interpretation.
For instance
@Begin(Display)
@st<char magic_header[] = { "\037\235" };>
@End(Display)
is acceptable to PCC, but not to ZETA-C, which insists on either
@Begin(Display)
@st<char magic_header[] = {'\037', '\235', '\0'};>
  or
@st<char magic_header[] = "\037\235";>
@End(Display)

As Guy Steele himself says (personal communication):

@Begin(Quotation)
It is a very curious thing that a construct can be ILLEGAL and nevertheless highly portable!  That comes from many
implementations being based on the same widely-distributed but incorrect source code.

The code that processes braces in PCC is rather peculiar and @i<ad hoc>.  It accepts what K&R specifies, but also accepts
many other cases of missing or extraneous braces in a rather idiosyncratic pattern.  In my opinion this particular use
of braces is logically extraneous and should be avoided for maximum portability.  The braces ought to be interpreted
as being a list of items to be used to fill in the array @lt<magic_header>, which ought to be of length 1 because that is the
length of the brace-list.  That would be the correct interpretation of
@Begin(Display)
@st<char *magic_header[] = { "\037\235" };>
@End(Display)
However, without the "*" one gets the situation of trying to initialize a @lt<char> to a pointer value, and PCC goes to
some trouble to figure out what you really might have meant.
@End(Quotation)

ZETA-C allows any expression to be used in an initializer, even for variables of static extent.

@ReSection(4.6.8  Other Types)

Objects of type @lt<lispval> are also initializable.  Of course, an initializer expression for such an object must be either a
variable of type @lt<lispval>, a Lisp expression introduced with the @lt<@@> operator (page @Ref(LispInclPage)), or any expression
explicitly cast to type @lt<lispval>.

@ReSection(4.8  EXTERNAL NAMES)@Index(external names)

The approach ZETA-C takes to the resolution of external names is the "omitted-@lt<extern>" solution described here.


@ReSection(@Big(5  Types))

@ReSection(5.1  STORAGE UNITS)

See the discussion in the section on Data Structures that begins on page @Ref(DataStructuresPage).

@ReSection(5.2  INTEGER TYPES)@Index(integer types)

@lt<int> and @lt<unsigned> are all potentially infinite precision (bignums).  See the discussion on Numbers on page
@Ref(NumbersPage).

@ReSection(5.2.3  CHARACTER TYPE)@Index(character types)

The type @lt<char> is unsigned.  The type @lt<signed char> is available if requested explicitly; signed chars are slower, however
(see page @Ref(NumbersPage)).  Note that some of the characters in the LispM's character set are between 128 and 255
decimal, so they require 8 bits to represent, and will appear negative if assigned to a @lt<signed char> variable.

@ReSection(5.4  POINTER TYPES)

@ReSection(5.4.2  Some problems with pointers)

See @Sec@:6.7.1 below, concerning alignment behavior.

@ReSection(5.6  ENUMERATION TYPES)@Index(enumeration types)

Enumeration tags are actually in a separate overloading class from structure and union tags.

@Comment{??? Do redeclarations such as on p. 102 cause reasonable error messages?  Expressions such as in the "enum
boys" example don't work, although they probably could (one gets an unbound-variable error).}

@ReSection(5.6.1  Detailed Semantics)@Index(enumeration semantics)

ZETA-C uses the integer model for enumerations (see @i<C:ARM>, p. 103).  That is, all enumeration types are treated as
synonyms for the type @lt<int>; enumeration constants and variables may be mixed freely with integers in expressions.

@ReSection(5.7  STRUCTURE TYPES)@Index(structures)

@ReSection(5.7.1  Operations on Structures)@Tag(FirstclassStructuresPage = Page)@Index(struct, assignment)
@Index(struct, returning)@Index(assignment of struct)@Index(firstclass structures)

Treating structures (and unions) as firstclass objects -- assigning them, passing them as parameters, and returning them
as values -- are all supported under ZETA-C.  However, since some compilers do not support these operations, we have
provided an option to "turn them off", causing ZETA-C to signal an error if one of these things is attempted.  Just put
the line @Display{@st<#define ZETA_C_NO_FIRSTCLASS_STRUCTURES>} at the beginning of your program.  Firstclass
structures are supported by most of the UNIX compilers, but by fewer of the non-UNIX compilers, so for maximum
portability they should not be used.

@ReSection(5.7.2  Components)@Index(struct and union components)

ZETA-C@Tag(StructNamespacePage = Page) follows the "modern" rule described here, that each structure type defines a
separate namespace for its components.  That is, component names may be reused freely in different structure types
(though of course they must be unique within a single structure type).

@ReSection(5.7.4  Bit Fields)@Index(bit fields)

Bit fields are allocated from right to left.  They may be signed or unsigned.  @Case(Brand, LMI "The maximum width
of any single field is 24 bits.")

@ReSection(5.11 TYPEDEF NAMES)@Index(typedef names)

@ReSection(5.11.1  Redefining Typedef Names)

ZETA-C does not correctly handle the example given in this section.  (This is pretty questionable programming practice,
anyway.)


@ReSection(@Big(6  Type Conversions))

@ReSection(6.3  CONVERSIONS TO INTEGER TYPES)@Index(Integer types)

Traditional architectures provide different instructions for operating on different kinds of numbers; it is up to the
compiler to keep track of what kind of number is involved in any particular operation, and to generate type conversion
instructions as necessary.  Lisp Machines, on the other hand, support generic arithmetic instructions, and do type
conversion automatically at runtime.  This can cause unexpected behavior in one case: if a variable is declared @lt<int>, say,
but is given a floating-point value by way of an escape to Lisp (see page @Ref(LispInclPage)), computations performed
using that value will yield floating-point results.  These may be stored in other @lt<int> variables, and so forth, propagating
floating-point-ness throughout large parts of the program's data structures.  So be careful at the interface between Lisp
and C that numeric values match their declared types.  (As long as one stays entirely within C, the compiler will
warn about mismatches.)

@ReSection(6.3.2  From Floating-point Types)@Index(float to int conversion)@Index(conversion, float to int)

When floating-point numbers are converted to integers, negative numbers are truncated downward (away from 0).

@ReSection(6.3.4  From Pointer Types)

See @Sec@:6.7.2 below.

@ReSection(6.7  CONVERSIONS TO POINTER TYPES)

@ReSection(6.7.1  From Pointer Types)@Index(pointer type conversion)@Index(conversion, pointer to pointer)

A ZETA-C pointer will be in one of three different representations, called "scales".  Recall (page @Ref(PointersPage))
that a pointer is a pair of an array and an index.  The index will be that used by @lt<aref> to access the element the
pointer points to; where this is in memory depends, of course, on whether the array is of type @lt<art-q>, @lt<art-16b>, or
@lt<art-8b>.  So, when a pointer is cast to a pointer type that uses a different array representation, the index has to be
"rescaled"; for instance, conversion from a pointer to @lt<short> to a pointer to @lt<int> requires dividing the index by 2.  The
effect is that when a pointer of a smaller scale is converted to a larger scale, it is automatically aligned for the larger
scale; in the example just given, the information about @i<which> halfword in the word the @lt<short>-pointer pointed to is lost.

@ReSection(6.7.2  From Integer Types)@Index(pointer to int conversion)@Index(int to pointer conversion)
@Index(conversion, pointer to int)@Index(conversion, int to pointer)

Conversions between integers and pointers have the following complicated behavior, which does a pretty good job of
simulating conventional implementations in certain important cases:

@Begin(Itemize)
When an integer is converted to a pointer, a runtime check is made:

@Begin(Itemize)
If the integer-typed object actually is an integer, the resulting pointer has array @lt<NIL> and index whatever the integer
was.

If the integer-typed object actually contains a Lisp cons (see below), the array part of the resulting pointer is the @lt<car> of
that cons, and the index part is its @lt<cdr>.
@End(Itemize)

Similarly, when a pointer is converted to an integer:

@Begin(Itemize)
If the array part of the pointer is @lt<NIL>, the resulting integer is just the index part of the pointer.

Otherwise, the result is @lt<cons> of the array part and the index part.  A cons, of course, is not an integer, so an attempt to
do arithmetic on it will cause a wrong-argument-type error.  This is how the second case mentioned above can arise.
@End(Itemize)
@End(Itemize)

The result of this scheme is 1) casting @lt<0> to a pointer always gives the null pointer; 2) casting a null pointer to an
integer always gives @lt<0>; 3) one can cast an integer to a pointer and back without loss of information, though the pointer
thus created cannot be dereferenced; and 4) one can cast a pointer to an integer and back without loss of information,
though this causes consing at runtime and the "integer" cannot be used for arithmetic.  Note, incidentally, that
assignment of an integer to a pointer or conversely will generate a compile-time warning that a cast is being performed.

@ReSection(6.11  THE ASSIGNMENT CONVERSIONS)@Index(assignment conversions)

If the left and right side types of an assignment expression are not one of the combinations specified here, ZETA-C will
issue a warning message to the effect that a cast is being attempted.  If the cast is legal, compilation proceeds; otherwise
(say, if an attempt is made to assign a @lt<struct> to an @lt<int>), an error is issued.

@ReSection(6.12  THE USUAL UNARY CONVERSIONS)@Index(conversion, implicit double to float)

An optional compilation mode is provided to suppress the implicit conversion of @lt<float>s to @lt<double>s (see p.
@Ref(FastFloatPage)).  Just put the line @Display{@st<#define ZETA_C_SUPPRESS_AUTO_FLOAT_TO_DOUBLE>} at the
beginning of your program.

@ReSection(6.14  THE FUNCTION ARGUMENT CONVERSIONS)@Index(conversion, function argument)

The ZETA-C math library routines will accept arguments of type either @lt<float> or @lt<double>, and will return results of the
corresponding type.

ZETA-C's representation of the null pointer is not the same as that of the number 0 (see page @Ref(PointersPage)).
Therefore, when a constant null pointer is to be passed as an argument to a function, the number 0 must be explicitly
cast to the appropriate type; for instance,

@Display{@st<foo(bar, (char *)0, quux)>}

If the cast is omitted, ZETA-C's function-argument type checker will issue a warning.

It is traditional (and good style) to define @lt<NULL> to the null pointer expression in the preprocessor, and use it everywhere
instead of the constant 0:

@Display{
@st<#define NULL ((char *) 0)
 >...@st< foo(bar, NULL, quux)>
}

It is less traditional, and even better style, to have several different null pointers, one for each pointer type in one's
program:

@Display{
@st<#define CNULL ((char *) 0)
#define FNULL ((struct foo *) 0)>
... etc. ...
}


@ReSection(@Big(7  Expressions))

@ReSection(7.2  EXPRESSIONS AND PRECEDENCE)

@ReSection(7.2.3  Overflow and Other Arithmetic Exceptions)

Because of the infinite-precision integer ("bignum") facility of the Lisp Machine, integer addition or multiplication
cannot overflow.  Division by zero and floating-point overflow will cause a Lisp error to be signalled (see your Lisp
manual for details); by default, this will invoke the Debugger.  Concerning floating-point underflow, see the description
of the Lisp variable @lt<zunderflow> in your Lisp manual.

@ReSection(7.5  BINARY OPERATOR EXPRESSIONS)

@ReSection(7.5.1  Multiplicative Operators)@Index(truncation in integer division)
@Index(@w<integer division, truncation in>)

Lisp Machine integer division truncates toward zero, rather than toward negative infinity; so, for example:
@Begin(Display)
@st<5 / 3  @lt<=>>  1
-5 / 3 @lt<=>>  -1
5 % 3  @lt<=>>  2
-5 % 3 @lt<=>>  -2>
@End(Display)

@ReSection(7.5.2  Additive Operators)@Index(pointer subtraction)@Index(@w<subtraction, pointer>)

When two pointers are being subtracted, ZETA-C checks at @i<runtime> that they point into the same array, and signals an
error if not (the message is "@st<Can't subtract pointers into different arrays, @it<array1> and @it<array2>>").

@ReSection(7.5.3  Shift Operators)@Index(shift operators)

Right shifts of a signed value shift in copies of the sign bit at the left.  What may seem surprising is that the same is
true of right shifts of full-length @i<unsigned> values; but recall that these are effectively infinitely long, and so even if
0-bits were being shifted in at the "left", they would never become visible (see p. @Ref(NumbersPage)).  Right shifts of
(@lt<unsigned>) @lt<char>s and of @lt<unsigned short>s, on the other hand, shift 0-bits in at the left, as the language specifies and
one would expect.

@ReSection(7.5.4  Inequality Operators)@Index(inequality operators)@Index(pointer comparison)
@Index(@w<comparison, pointer>)

Note that comparison of pointers into different top-level aggregates (see the discussion of pointers and aggregates starting
on p. @Ref(PointersPage)) cannot yield consistent results on the Lisp Machine, since Lisp's garbage collector can change
the order of arrays in memory at runtime.  We deal with this as follows.  By default, the array parts of pointers are
ignored in comparisons.  (We have encountered C code that in fact compares pointers to different arrays, but doesn't
care what the result of the comparison is in that case.)  However, ZETA-C provides an option to cause comparison of
pointers into different arrays to signal an error at runtime.  If you wish such errors signalled, put the line
@Display{@st<#define ZETA_C_COMPARE_POINTERS_CAREFULLY>} at the beginning of your program.

@ReSection(7.8  ASSIGNMENT EXPRESSIONS)@Index(assignment expressions)

@ReSection(7.8.2  Compound Assignment)@Index(compound assignment operators)

ZETA-C requires compound assignment operators to be written as single syntactic tokens; for example, "@lt<&=>" rather than
@w<"@lt<& =>">.

@ReSection(7.11  ORDER OF EVALUATION)@Index(order of evaluation)@Index(@w<evaluation, order of>)

ZETA-C, like Lisp, evaluates the arguments of a function call in left-to-right order; but don't count on this, since most
C compilers evaluate function arguments right-to-left!  ZETA-C does not currently rearrange expressions in any of the
ways described in this section of @i<C:ARM>, but we do not guarantee that it will not do so in the future.


@ReSection(@Big(8  Statements))

@ReSection(8.4  COMPOUND STATEMENT)@Index(compound statement)@Index(goto statement)@Index(labels)

As mentioned in @Sec@:4.2.1 above, ZETA-C does not support jumping to a labeled statement within a compound
statement when the compound statement has declarations of @lt<auto> or @lt<register> variables.  This very reasonable
restriction is enforced by the Lisp compiler.


@ReSection(8.7  SWITCH STATEMENT; CASE AND DEFAULT LABELS)@Index(switch statement)

ZETA-C requires that the body of a @lt<switch> statement be a compound statement, with no declarations.  Furthermore,
@lt<case> or @lt<default> labels may not appear in any compound statement inside the @lt<switch> body if that compound statement
declares @lt<auto> or @lt<register> variables.


@ReSection(@Big(9  Functions))

@ReSection(9.5  AGREEMENT OF FORMAL AND ACTUAL PARAMETERS)@Index(function type checking)
@Index(@w<type checking, function>)

ZETA-C checks the types of actual parameters ("arguments") against the types of the corresponding formal parameters at
both compile time and load time, issuing warnings if they do not match.  Since the check is done at load time,
mismatches will be noticed even when the caller and callee are in different source files.  Also, while normally ZETA-C
conflates all the integral types -- @lt<int>s, @lt<long>s, @lt<unsigned>s, etc., may be mixed freely in arithmetic and assignment
expressions -- these types are all considered distinct for the purpose of parameter type checking (except that the @lt<short>
and @lt<char> types are automatically widened to @lt<int> or @lt<unsigned>, as appropriate).  This is because most C compilers cannot
do type-checking in this case, and (depending on the details of the C implementation) the different types cannot be
counted on to be the same length.  If a traditional implementation passes function arguments on the stack, and say @lt<int>
and @lt<long> are different sizes, and an @lt<int> is passed where a @lt<long> is expected, the arguments will not be aligned as the
callee expects, causing erroneous behavior.  So, to help the user prevent such errors, ZETA-C here provides some of the
functionality of the UNIX @b<lint> utility.

@ReSection(9.8  AGREEMENT OF ACTUAL AND DECLARED RETURN TYPE)@Index(returning no value)

A @lt<return> statement with no expression causes the Lisp object @st<(:|No value returned from| @it<function>)> to be returned,
where @it<function> is the name of the function.  Any attempt to do arithmetic, pointer operations, @i<etc.> on such an object
will of course signal an error, whereupon it will be obvious that @it<function> was expected to return a value but didn't.

If the type of the expression in an @lt<return> statement is not convertible by assignment to the declared return type of the
function, ZETA-C will issue a warning message to the effect that a casting conversion is being attempted.  If that fails,
an error is issued.





@Chapter(Library routines)@Tag(LibraryChapter = Chapter)

This chapter lists the I/O routines and "system calls" provided by ZETA-C.  We have attempted to provide a library
complete enough and compatible enough that most "user-level" UNIX programs will run with few or no changes.  We
have not, however, supported multiprocessing, pipes, raw device I/O, or some of the more arcane operations UNIX
provides on file descriptors.  Also, note that most versions of Lisp Machine software do not support bidirectional file
streams.

All of these functions are interned in package @lt<C:>.


@Section(File and stream I/O)

UNIX provides two levels of file handling routines.  One consists of system calls, with which one communicates in terms
of "file descriptors", which are (as it happens) magic numbers whose meaning is known only to the kernel; the system
calls have names like @lt<open>, @lt<close>, @lt<read>, @lt<write>.  The other is a collection of library routines, known as the "standard
I/O package", which talk in terms of "streams", which are pointers to structures of type @lt<FILE> (defined in @lt<stdio.h>);
most of these routines' names begin with "f": @lt<fopen>, @lt<fclose>, @lt<fread> (but note @lt<putc>, @lt<getc>).  Consult a UNIX manual for
the details; we just wish to point out here that ZETA-C makes a distinction between file descriptors and streams, and
will give you an error if you call a routine with the wrong kind of thing.  (The implementation difference is just that a
stream is a cons of a file descriptor and @lt<NIL>.)

Note that a file name is anything acceptable to @lt<fs:parse-pathname>.


@SubSection(Kernel level I/O)

@FunctionDoc(Name "@st<int @lt<open>>@Index(@st<open>)", Arglist "@it<name mode>", Decls "@st<char *name;@*int mode;>", Doc "Opens the
file @it<name> for reading (if @it<mode> is 0), writing (if @it<mode> is 1), or both (if @it<mode> is 2).  Returns a file descriptor, or -1 on error.
@Case(Class, LMITI "The Lisp Machine file software does not support bidirectional file I/O, and will signal an error if
@it<mode> is 2.")")

@FunctionDoc(Name "@st<int @lt<creat>>@Index(@st<creat>)", Arglist "@it<name mode>", Decls "@st<char *name;@*int mode;>", Doc "Creates
a file named @it<name>.  @it<mode> is ignored.  Returns a file descriptor, or -1 on error.")

@FunctionDoc(Name "@lt<close>@Index(@st<close>)", Arglist "@it<fd>", Decls "@st<int kstream;>", Doc "Closes the file on file descriptor
@it<fd>.")

@FunctionDoc(Name "@st<int @lt<read>>@Index(@st<read>)", Arglist "@it<fd buffer nbytes>", Decls "@st<int fd, nbytes;@*char *buffer;>",
Doc "Reads up to @it<nbytes> bytes from file descriptor @it<fd>, putting the result in @it<buffer>.  Returns the number of bytes read,
which will be less than @it<nbytes> if end-of-file was encountered; or -1 on error.")

@FunctionDoc(Name "@st<int @lt<write>>@Index(@st<write>)", Arglist "@it<fd buffer nbytes>", Decls "@st<int fd, nbytes;@*char
*buffer;>", Doc "Writes @it<nbytes> bytes to file descriptor @it<fd> from @it<buffer>.  Returns the number of bytes written, or -1 on
error.")

@FunctionDoc(Name "@st<long @lt<lseek>>@Index(@st<lseek>)", Arglist "@it<fd offset whence>", Decls "@st<int fd, whence;@*long
offset;>", Doc "Sets the read/write pointer of file descriptor @it<fd> to @it<offset>, if @it<whence> is 0; or to the current location plus
@it<offset>, if @it<whence> is 1; or to the end of the file plus @it<offset>, if @it<whence> is 2.  If the resulting position is before the
beginning of the file, does nothing and returns -1; else returns the new value of the pointer.")

@FunctionDoc(Name "@st<long @lt<tell>>@Index(@st<tell>)", Arglist "@it<fd>", Decls "@st<int fd;>", Doc "Returns the current value of the
read/write pointer for file descriptor @it<fd>.")

@FunctionDoc(Name "@st<int @lt<isatty>>@Index(@st<isatty>)", Arglist "@it<fd>", Decls "@st<int fd;>", Doc "Returns @b<true> (nonzero) iff file
descriptor @it<fd> is@st< >associated with the user's console.")

UNIX normally provides input line buffering with simple editing on streams to the user's console.  ZETA-C defines the
flavor @lt<zeta-c:unix-terminal-io-stream> to provide the same functionality (see @st<zeta-c:source;zcio.lisp> if you're
curious about the details).  While many of the various modes, line speeds, etc. that Unix provides its terminal streams
are meaningless on the Lisp Machine's direct-connect terminal, ZETA-C does implement the following:

@Begin(Description)
CBREAK mode@\When active, turns off line editing, so that each character is returned when typed.  @i<Off> by default.

ECHO mode@\When active, causes each character to be echoed as typed.  @i<On> by default.
@End(Description)

The following functions are available to set and read these modes:

@FunctionDoc(Name "@st<int @lt<gtty>>@Index(@st<gtty>)", Arglist "@it<fd sgttyb>", Decls "@st<#include <sgtty.h>
int fd;
struct sgttyb *sgttyb;>", Doc "Gets the CBREAK and ECHO modes of the terminal stream associated with @it<fd>, and sets
bits in @st<sgttyb.sg_flags> appropriately (see @st<zeta-c:include;sgtty.h>).  Note that these are the only modes read.
A Lisp error is signalled if @it<fd> is not associated with a terminal stream.  Returns 0.")

@FunctionDoc(Name "@st<int @lt<stty>>@Index(@st<stty>)", Arglist "@it<fd sgttyb >&optional@it< dont_flush>", Decls "@st<#include <sgtty.h>
int fd, dont_flush;
struct sgttyb *sgttyb;>", Doc "Sets the CBREAK and ECHO modes of the terminal stream associated with @it<fd> according
to bits in @st<sgttyb.sg_flags> (see @st<sgtty.h>).  CBREAK mode is substituted for RAW mode.  Note that these are the only
modes set.  First clears all input from the stream and waits for pending output to complete, unless @it<dont_flush> is @b<true>
(nonzero).  A Lisp error is signalled if @it<fd> is not associated with a terminal stream.  Returns 0.")

@FunctionDoc(Name "@st<void @lt<ioctl>>@Index(@st<ioctl>)", Arglist "@it<fd opcode thing>", Decls "@st<#include <sgtty.h>
int fd, opcode;
struct sgttyb *thing;>", Doc "Does one of several things depending on @it<opcode>.  The operations available are:
@Begin(Description)
@st<TIOCGETP>@\Equivalent to @st<gtty(@it<fd>, @it<thing>)>.

@st<TIOCSETP>@\Equivalent to @st<stty(@it<fd>, @it<thing>)>.

@st<TIOCSETN>@\Equivalent to @st<stty(@it<fd>, @it<thing>, 1)> (doesn't flush the stream before setting modes).

@st<FIONREAD>@\In this case, @it<thing> is expected to be a pointer to @lt<int>.  @st<*@it<thing>> is set to 1 if there are characters available to
be read from @it<fd>, otherwise 0.
@End(Description)
A Lisp error is signalled if @it<fd> is not associated with a terminal stream.  Returns 0.")

@FunctionDoc(Name "@st<void @lt<ttytimeout>>@Index(@st<ttytimeout>)", Arglist "@it<fd timeout>", Decls "@st<int fd, timeout;>", Doc
"Waits up to @it<timeout>/60 seconds for input to be available on the terminal stream associated with @it<fd>.  That is, it returns
when input is available or when the time runs out, whichever happens first.  If a character is available, it can be read
with @st<getc> or possibly @st<getchar>.")


@Subsection(Stdio level I/O)

To use any of these functions, add the line
@Display{@st<#include <stdio.h>>}
at the beginning of any relevant files.  Among other things, this file defines the following variables:

@VariableDoc(Name "@st<FILE *@lt<stdin>>@Index(@st<stdin>)", Doc "The stream which is bound to the standard input (literally, to
the value@st< >of the Lisp variable @lt<standard-input> at the point the program was invoked.)")

@VariableDoc(Name "@st<FILE *@lt<stdout>>@Index(@st<stdout>)", Doc "The stream which is bound to the standard output (literally,
to the value@st< >of the Lisp variable @lt<standard-output> at the point the program was invoked.)")

@VariableDoc(Name "@st<FILE *@lt<stderr>>@Index(@st<stderr>)", Doc "The stream which is bound to the standard error output
(literally, to the@st< >value of the Lisp variable @lt<error-output> at the point the program was invoked.)")

The standard I/O package provides the following functions:

@FunctionDoc(Name "@st<FILE *@lt<fopen>>@Index(@st<fopen>)", Arglist "@it<name mode>", Decls "@st<char *name, *mode;>", Doc {Opens the
file @it<name> for@st< >reading (if @it<mode> is "r"), creates it for writing (if @it<mode> is "w"), or opens it for appending (if @it<mode> is "a").
Returns a stdio@st< >stream, or NULL on error.})

@FunctionDoc(Name "@lt<fclose>@Index(@st<fclose>)", Arglist "@it<stream>", Decls "@st<FILE *stream;>", Doc "Closes the stream @it<stream>.")

@FunctionDoc(Name "@lt<fflush>@Index(@st<fflush>)", Arglist "@it<stream>", Decls "@st<FILE *stream;>", Doc "Causes any buffered data
for the output@st< >stream @it<stream> to be written out.  The stream remains open.")

@FunctionDoc(Name "@st<int @lt<feof>>@Index(@st<feof>)", Arglist "@it<stream>", Decls "@st<FILE *stream;>", Doc "Returns @b<true> (nonzero)
iff the input@st< >stream @it<stream> is at end-of-file.")

@FunctionDoc(Name "@st<int @lt<getc>>@Index(@st<getc>)", Arglist "@it<stream>", Decls "@st<FILE *stream;>", Doc "Reads and returns the
next character from @it<stream>; or, if end-of-file is encountered, returns @lt<EOF> (which is an integer value not equal to any
character; be sure to assign the result of @lt<getc> to an @lt<int>, not @lt<char>, variable).")

@FunctionDoc(Name "@st<int @lt<getchar>>@Index(@st<getchar>)", Arglist "", Decls "", Doc "Equivalent to @st<getc(stdin)>.")

@FunctionDoc(Name "@st<int @lt<fgetc>>@Index(@st<fgetc>)", Arglist "@it<stream>", Decls "@st<FILE *stream;>", Doc "A synonym of @lt<getc>
(see above).")

@FunctionDoc(Name "@lt<ungetc>@Index(@st<ungetc>)", Arglist "@it<c stream>", Decls "@st<char c;
FILE *stream;>", Doc "Un-gets the previously read character @it<c> from @it<stream>, so that it will be returned by the next
call to @lt<getc>.  Only one character may be ungotten at a time, and it must be the same as the last character read.")

@FunctionDoc(Name "@st<int @lt<getw>>@Index(@st<getw>)", Arglist "@it<stream>", Decls "@st<FILE *stream>;", Doc "Reads the next two bytes
from @it<stream>, assembles them into a word, and returns the result.  Does not assume, nor enforce, any special alignment.
Returns @lt<EOF> if end-of-file is encountered.")

@FunctionDoc(Name "@st<char *@lt<gets>>@Index(@st<gets>)", Arglist "@it<s>", Decls "@st<char *s;>", Doc "Reads a line from @lt<stdin> into the
character array @it<s>.  Does not include the trailing newline.  Returns @lt<NULL> if end-of-file was encountered at the beginning
of the line.")

@FunctionDoc(Name "@st<char *@lt<fgets>>@Index(@st<fgets>)", Arglist "@it<s n stream>", Decls "@st<char *s;
int n;
FILE *stream;>", Doc "Reads a line from @it<stream> into the character array @it<s>.  Reads at most @it<n> - 1 characters before
returning.  The line includes the trailing newline, if one was read.  Returns @lt<NULL> if end-of-file was encountered
at the beginning of the line.")

@FunctionDoc(Name "@lt<putc>@Index(@st<putc>)", Arglist "@it<ch stream>", Decls "@st<char ch;
FILE *stream;>", Doc "Writes the character @it<ch> to @it<stream>.")

@FunctionDoc(Name "@lt<putchar>@Index(@st<putchar>)", Arglist "@it<ch>", Decls "@st<char ch;>", Doc "Equivalent to @st<putc(@it<ch>,stdout)>.")

@FunctionDoc(Name "@lt<fputc>@Index(@st<fputc>)", Arglist "@it<ch stream>", Decls "@st<char ch;
FILE *stream;>", Doc "A synonym of @lt<putc> (see above).")

@FunctionDoc(Name "@lt<putw>@Index(@st<putw>)", Arglist "@it<w stream>", Decls "@st<int w;
FILE *stream;>",  Doc {Writes the 16-bit "word" @it<w> to @it<stream> as a pair of bytes, in such a way that @lt<getw> will read the
same word back.})

@FunctionDoc(Name "@lt<puts>@Index(@st<puts>)", Arglist "@it<s>", Decls "@st<char *s;>", Doc "Writes the @lt<NUL>-terminated string @it<s> to @lt<stdout>,
appending a newline character.")

@FunctionDoc(Name "@lt<fputs>@Index(@st<fputs>)", Arglist "@it<s stream>", Decls "@st<char *s;
FILE *stream;>", Doc "Writes the @lt<NUL>-terminated string @it<s> to @it<stream>.  (Does not append a newline.)")

@FunctionDoc(Name "@st<int @lt<fread>>@Index(@st<fread>)", Arglist "@it<buffer itemsize itemcount stream>", Decls "@st<char *buffer;
int itemsize, itemcount;
FILE *stream;>", Doc "Reads @it<itemcount> items, each @it<itemsize> bytes long, from @it<stream> into @it<buffer>.  Returns the number
of items actually read; this will be smaller than @it<itemcount> if end-of-file was encountered.")

@FunctionDoc(Name "@lt<fwrite>@Index(@st<fwrite>)", Arglist "@it<buffer itemsize itemcount stream>", Decls "@st<int *buffer;
int itemsize, itemcount;
FILE *stream;>", Doc "Writes @it<itemcount> items, each @it<itemsize> bytes long, to @it<stream> from @it<buffer>.")

@FunctionDoc(Name "@st<int @lt<fseek>>@Index(@st<fseek>)", Arglist "@it<stream offset whence>", Decls "@st<FILE *stream;
long offset;
int whence;>", Doc "Sets the read/write pointer of @it<stream> to @it<offset>, if @it<whence> is 0; or to the current location plus @it<offset>,
if @it<whence> is 1; or to the end of the file plus @it<offset>, if @it<whence> is 2.  If the resulting position is before the beginning of the
file, does nothing and returns -1; else returns the new value of the pointer.")

@FunctionDoc(Name "@st<int @lt<ftell>>@Index(@st<ftell>)", Arglist "@it<stream>", Decls "@st<FILE *stream;>", Doc {Returns the current
value of the read/write pointer for @it<stream>.  Note that on some file systems, this value is a "magic cookie" which is only
meaningful to @lt<fseek>.})

@FunctionDoc(Name "@lt<frewind>@Index(@st<frewind>)", Arglist "@it<stream>", Decls "@st<FILE *stream;>", Doc "Sets the read-write
pointer for @it<stream> to the beginning of the file.")


@SubSection(Miscellaneous file operations)

@FunctionDoc(Name "@lt<unlink>@Index(@st<unlink>)", Arglist "@it<name>", Decls "@st<char *name;>", Doc "Deletes the file named @it<name>.
Does not expunge it, so if the file system supports undeletion, it can be undeleted.")

@FunctionDoc(Name "@lt<chdir>@Index(@st<chdir>)", Arglist "@it<newdir>", Decls "@st<char *newdir;>", Doc "Changes the current
directory (and, optionally, host and device) to that specified by @it<newdir>.  Note that @it<newdir> must be recognizable by
@lt<fs:parse-pathname> as containing a directory.")


@SubSection(Formatted output)

These functions all interpret their @it<fmt> argument as described below.

@FunctionDoc(Name "@lt<printf>@Index(@st<printf>)", Arglist "@it<fmt> &rest @it<args>", Decls "@st<char *fmt;>", Doc "Prints each of the
@it<args> on @lt<stdout> according to @it<fmt> (see below).")

@FunctionDoc(Name "@lt<fprintf>@Index(@st<fprintf>)", Arglist "@it<stream fmt> &rest@it< args>", Decls "@st<#include <stdio.h>
FILE *stream;
char *fmt;>", Doc "Prints each of the @it<args> on @it<stream> according to @it<fmt> (see below.)")

@FunctionDoc(Name "@lt<sprintf>@Index(@st<sprintf>)", Arglist "@it<string fmt> &rest @it<args>", Decls "@st<char *string, *fmt;>", Doc
{"Prints" each of the @it<args> according to @it<fmt> (see below), placing the output in the character array @it<string>; appends a
@lt<NUL> at the end.})

Characters in the format string @it<fmt> are just copied to the output, except for @lt<%>, which introduces a formatting directive.
A directive has the following syntax:

@Display{@st<%>[@st<->][@st<0>][@it<width>][.@it<precision>][@st<l>]@it<conv>}

@it<width> and @it<precision> are both decimal integers.  If a minus sign appears before @it<width>, the converted value is left justified
in the field; if a zero appears before @it<width>, padding will be done with zeros instead of blanks.  @it<conv> may be one of the
following:

@Begin(Description, indent -1 in)
@lt<d o x>@\The integer @it<arg> is printed in decimal, octal, or hex respectively.

@lt<f g>@\The float @it<arg> is printed in the style `[-]@st<ddd.ddd>' where the number of d's is @it<precision> (default 6); exponential
notation is used in case of underflow or overflow.

@lt<e>@\The float @it<arg> is printed in the style `[-]@st<d.ddddde>[-]@st<dd>' where the number of d's is @it<precision> (default 6).

@lt<s>@\The @lt<NUL>-terminated string @it<arg> is printed; if @it<precision> is specified, it is the maximum number of characters to print.

@lt<c>@\The character @it<arg> is printed.  @lt<NUL> is ignored.
@End(Description)

@it<conv> may be in either case.  An '@lt<l>' before @it<conv> is ignored.

Note that the floating-point conversions do not behave identically to those of UNIX @lt<printf>: there is no distinction
between the @lt<f> and @lt<g> conversions, and the @it<precision> is the @b<total> number of digits rather than the number of digits after
the decimal point.


@Section(Formatted input)

These functions all interpret their @it<fmt> argument as described below.

@FunctionDoc(Name "@st<int @lt<scanf>>@Index(@st<scanf>)", Arglist "@it<fmt> &rest @it<pointers>", Decls "@st<char *fmt;>", Doc
"Reads input from @lt<stdin>, interprets it according to @it<fmt>, and stores the resulting values through the corresponding
@it<pointers>.")

@FunctionDoc(Name "@st<int @lt<fscanf>>@Index(@st<fscanf>)", Arglist "@it<stream fmt> &rest @it<pointers>", Decls "@st<FILE *stream;
char *fmt;>", Doc "Reads input from @it<stream>, interprets it according to @it<fmt>, and stores the resulting values through the
corresponding @it<pointers>.")

@FunctionDoc(Name "@st<int @lt<sscanf>>@Index(@st<sscanf>)", Arglist "@it<string fmt> &rest @it<pointers>", Decls 
"@st<char *string, *fmt;>", Doc "Reads input from the @lt<NUL>-terminated character array @it<string>, interprets it according to
@it<fmt>, and stores the resulting values through the corresponding @it<pointers>.")

[Documentation describing the interpretation of the @it<fmt> argument has not yet been written.  However, the full @lt<scanf>
functionality, as described in Harbison & Steele, is supported.]


@Section(String manipulation)

These functions work with @lt<NUL>-terminated strings.  Note that they will get array-bounds errors if the @lt<NUL> is missing, or
if there is not enough room to copy into.

@FunctionDoc(Name "@st<char *@lt<strcpy>>@Index(@st<strcpy>)", Arglist "@it<dest source>", Decls "@st<char *dest, *source;>", Doc "Copies
the string at @it<source> to @it<dest>.  (Note the order of the arguments; think of the function as being like assignment.)
Returns @it<dest>.")

@FunctionDoc(Name "@st<char *@lt<strcat>>@Index(@st<strcat>)", Arglist "@it<dest source>", Decls "@st<char *dest, *source;>", Doc
"Concatenates the string at @it<source> onto the end of that at @it<dest>.  (Note the order of the arguments; see @lt<strcpy> above.)
Returns @it<dest>.")

@FunctionDoc(Name "@st<int @lt<strlen>>@Index(@st<strlen>)", Arglist "@it<s>", Decls "@st<char *s;>", Doc "Finds the length of the string at
@it<s>.")

@FunctionDoc(Name "@st<int @lt<strcmp>>@Index(@st<strcmp>)", Arglist "@it<s1 s2>", Decls "@st<char *s1, *s2;>", Doc "Compares strings in
ASCII order, returning -1 if @it<s1> < @it<s2>, 0 if @it<s1> == @it<s2>, or 1 if @it<s1> > @it<s2>.")


@Section(Arithmetic and Transcendental Functions)

To use any of these functions, add the line
@Display{@st<#include <math.h>>}
at the beginning of the relevant files.

@FunctionDoc(Name "@st<int @lt<abs>>@Index(@st<abs>)", Arglist "@it<val>", Decls "@st<int val;>", Doc "Returns the absolute value of @it<val>.")

@FunctionDoc(Name "@st<double @lt<acos>>@Index(@st<acos>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the trigonometric inverse
cosine, in radians, of its argument.")

@FunctionDoc(Name "@st<double @lt<asin>>@Index(@st<asin>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the trigonometric inverse
sine, in radians, of its argument.")

@FunctionDoc(Name "@st<double @lt<atan>>@Index(@st<atan>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the trigonometric inverse
tangent, in radians, of its argument.  The result is between -@pi/2 and @pi/2.")

@FunctionDoc(Name "@st<double @lt<atan2>>@Index(@st<atan2>)", Arglist "@it<y x>", Decls "@st<double y, x;>", Doc "Returns the angle part
of the polar-coordinate representation of the point (@it<x>, @it<y>); that is, the angle between the positive x-axis and a ray drawn
from the origin through the point (@it<x>, @it<y>).  The result is in radians and is between -@pi and @pi.")

@FunctionDoc(Name "@st<double @lt<atof>>@Index(@st<atof>)", Arglist "@it<str>", Decls "@st<char *str;>", Doc "Converts a string of numeric
characters to a floating-point number.  Recognizes an optional string of tabs and spaces, then an optional sign, then a
string of digits optionally containing a decimal point, then an optional 'e' or 'E' followed by an optionally signed integer.
The first unrecognized character ends the string.")

@FunctionDoc(Name "@st<int @lt<atoi>>@Index(@st<atoi>)", Arglist "@it<str>", Decls "@st<char *str;>", Doc "Converts a string of numeric
characters to an @lt<int>.  Recognizes an optional string of tabs and spaces, then an optional sign, then a string of digits.
The first unrecognized character ends the string.")

@FunctionDoc(Name "@st<long @lt<atol>>@Index(@st<atol>)", Arglist "@it<str>", Decls "@st<char *str;>", Doc "Since there's no distinction in
ZETA-C between an @lt<int> and a @lt<long>, this is effectively a synonym for @lt<atoi>.")

@FunctionDoc(Name "@st<double @lt<ceil>>@Index(@st<ceil>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the smallest integral
value not less than @it<x>.")

@FunctionDoc(Name "@st<double @lt<cos>>@Index(@st<cos>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the trigonometric cosine of
its argument, which is assumed to be in radians.")

@FunctionDoc(Name "@st<double @lt<cosh>>@Index(@st<cosh>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the hyperbolic cosine of
its argument.")

@FunctionDoc(Name "@st<double @lt<exp>>@Index(@st<exp>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the exponential @it<e@+<x>>.")

@FunctionDoc(Name "@st<double @lt<fabs>>@Index(@st<fabs>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the absolute value of @it<x>.")

@FunctionDoc(Name "@st<double @lt<floor>>@Index(@st<floor>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the largest integral
value not greater than @it<x>.")

@FunctionDoc(Name "@st<double @lt<hypot>>@Index(@st<hypot>)", Arglist "@it<x y>", Decls "@st<double x, y>;", Doc "Returns the square root
of the sum of the squares of @it<x> and @it<y>.")

@FunctionDoc(Name "@st<double @lt<log>>@Index(@st<log>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the natural logarithm of @it<x>.")

@FunctionDoc(Name "@st<double @lt<log10>>@Index(@st<log10>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the base-10 logarithm
of @it<x>.")

@FunctionDoc(Name "@st<double @lt<pow>>@Index(@st<pow>)", Arglist "@it<x y>", Decls "@st<double x, y;>", Doc "Returns @it<x@+<y>>.")

@FunctionDoc(Name "@st<double @lt<sin>>@Index(@st<sin>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the trigonometric sine of
its argument, which is assumed to be in radians.")

@FunctionDoc(Name "@st<double @lt<sinh>>@Index(@st<sinh>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the hyperbolic sine of
its argument.")

@FunctionDoc(Name "@st<double @lt<sqrt>>@Index(@st<sqrt>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the square root of @it<x>.")

@FunctionDoc(Name "@st<double @lt<tan>>@Index(@st<tan>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the trigonometric tangent of
its argument, which is assumed to be in radians.")

@FunctionDoc(Name "@st<double @lt<tanh>>@Index(@st<tanh>)", Arglist "@it<x>", Decls "@st<double x;>", Doc "Returns the hyperbolic tangent of
its argument.")


@Section(Memory allocation)

@FunctionDoc(Name "@st<char *@lt<malloc>>@Index(@st<malloc>)", Arglist "@it<size>", Decls "@st<int size;>", Doc "Allocates a block of
memory @it<size> bytes long, and returns a pointer to the beginning of it.  This pointer can, of course, be cast to some other
type.  This works by calling the Lisp @lt<make-array>, and thus can only fail if you run entirely out of virtual memory --
but since all processes allocate from the same pool, this may well not be the fault of your C program.  The allocated
memory will contain zeros, but since other implementations' @lt<malloc> doesn't guarantee this, you shouldn't count on it.")

@FunctionDoc(Name "@st<char *@lt<calloc>>@Index(@st<calloc>)", Arglist "@it<nelem elsize>", Decls "@st<int nelem, elsize;>", Doc
"Allocates a block of@st< >memory @st<to hold @it<nelem>> objects each @it<elsize> bytes long, and returns a pointer to the beginning of
it.  The @i<first> thing you should do with this pointer is to cast it to the correct type; in fact, we recommend that you
call @lt<calloc> only by way of a macro like the following:
@Display{@st<#define aalloc(nelem, type)   ((type *)calloc(nelem, sizeof(type)))>@Foot{Unfortunately, this
only works for relatively simple types like "char *" and "struct foo"; it fails syntactically for more complex types like
"int (*)()", since the second asterisk should go @i<inside> the first pair of parentheses.  So for such cases you'll have to write
the calloc expression out in full.}}  ZETA-C explicitly recognizes such a construct, and when it allocates the block of
memory, initializes it correctly for the type that it is cast to (which is @i<not> the same as filling it with zeros, since the
null pointer is stored as a @lt<NIL> followed by a zero).  This works by calling the Lisp @lt<make-array>, and thus can only fail
if you run entirely out of virtual memory -- but since all processes allocate from the same pool, this may well not be
the fault of your C program.")

@FunctionDoc(Name "@st<char *@lt<realloc>>@Index(@st<realloc>)", Arglist "@it<object newsize>", Decls "@st<char *object;
int newsize;>", Doc "Allocates a block of memory @it<newsize> bytes long, and copies the contents of @it<object> into it (or as
much thereof as will fit, if the new object is smaller).  Returns a pointer to the beginning of the new object.")

@FunctionDoc(Name "@lt<free>@Index(@st<free>)", Arglist "@it<object>", Doc "Currently, does nothing.  (In the future, this will
optionally mark the object so that subsequent accesses to it will be trapped).")


@Section(Non-local exits)

The "functions" @lt<setjmp> and @lt<longjmp> are provided to allow a function to return control to an active function invocation
other than its caller's.  To use them, include the line
@Display{@st<#include <setjmp.h>>}
at the beginning of the relevant files.

@FunctionDoc(Name "@st<int @lt<setjmp>>@Index(@st<setjmp>)", Arglist "@it<env>", Decls "@st<jmp_buf env;>", Doc "Sets up a return point,
storing the information necessary to return to it in @it<env> for later use by @lt<longjmp>.  Returns 0.")

@FunctionDoc(Name "@st<void @lt<longjmp>>@Index(@st<longjmp>)", Arglist "@it<env val>", Decls "@st<jmp_buf env;
int val;>", Doc "Returns to the return point created by the last invocation of @lt<setjmp> on @it<env>.  Execution continues as if
that call to @lt<setjmp> had returned again, returning the value @it<val>.  (If @it<val> is 0, or if the function that called @lt<setjmp>
has already returned, @lt<longjmp> signals an error.)")


@Section(Program termination)

@FunctionDoc(Name "@lt<exit>@Index(@st<exit>)", Arglist "@it<status>", Decls "@st<int status;>", Doc "Exits the program immediately,
closing all files.  If @it<status> is nonzero, first offers to enter the debugger (a nonzero status is traditionally used to
indicate that the program encountered a fatal error).  This works by signalling @lt<sys:abort>.")

@FunctionDoc(Name "@lt<abort>@Index(@st<abort>)", Doc "Enters the debugger.  (On UNIX, this gives a core dump.)")

