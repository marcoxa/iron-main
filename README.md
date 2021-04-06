# IRON MAIN

The **mainframe** is, by definition, **iron**.  **IRON MAIN** is a
packaged set of utilities to make **Emacs** usable to edit (and
minimally interact) with a mainframe.

The *mainframe* is, for the time being, an IBM architecture
*machine* (or an emulator - see below) running a variant of MVS or
z/OS.

Some motivation: as of Fall 2020, there appeared to be no [*Job
Control Language*](https://en.wikipedia.org/wiki/Job_Control_Language)
(JCL) mode (that is, a `jcl-mode`) floating around the Emacs Internet
world.  Probably there was no need for it.  Yet.  So it seemed to be a
good idea to write one.

The result is this minimal collection of tools and modes in Emacs Lisp
to interact with the mainframe.

**IRON MAIN** comprises the following:

* **pl1-mode** (file `pl1-mode.el`): a major mode to handle PL/I code
  (see file for more information, history and credits).
* **asmibm-mode** (file `asmibm-mode.el`): a major mode to handle IBM
  Assemblers, at least in their barebone forms (no
  [HLASM](https://en.wikipedia.org/wiki/IBM_High_Level_Assembler) high
  level constructs yet).
* **jcl-mode** (file `jcl-mode.el`): a major mode to handle IBM MVS or Z/OS
  JCL.
* **jcl-poly-mode** (file `jcl-poly-mode.el`): a major mode to handle
  IBM MVS or Z/OS JCL based on `polymode.el'; *PL/I*, *COBOL*,
  *Fortran* and *ASM* are supported as *inner modes*.

  
Some tweaking was done to ensure that the column tracking and the
ruler (cfr., `ruler-mode`) that is used in the various editing modes
are 1-based, as expected on a "card".

The code has been tested with **MVS 3.8j TK4-** and **MVS 3.8j "Jay
Moseley"** build, running on a **Hercules** (**SDL/Hyperion 4.2.x**).
Useful links follow.

* **IBM z/OS**: <https://www.ibm.com/it-infrastructure/z/zos>
* **Hercules**: <http://www.hercules-390.eu/>
* **SDL/Hyperion**: <https://github.com/SDL-Hercules-390>
* **TK4-**: <http://wotho.ethz.ch/tk4-/>
* **Jay Moseley's site**: <http://www.jaymoseley.com/hercules/> (and
                          other useful things).
* **IBM Assemblers**:
  <https://en.wikipedia.org/wiki/IBM_Basic_Assembly_Language_and_successors>


## jcl-mode and jcl-poly-mode

**jcl-mode** and **jcl-poly-mode* are major modes to handle IBM MVS or
Z/OS JCL.

The basic "grammar" of JCL is the following, "line-oriented" one:

	//<name>? <operation> <operands> <comments>

or

	/*<comment>

everything starting at the `//` or `/*` and ending at the end of
line, or better at the 72nd column with whitespaces meaningful.
Actually, we are not even talking about "lines", but we are
talking about "cards" (as it should be).

In the following the naming just follows the above convention.

The mode, for the time being, allows you to write JCL files,
pardon, "data sets", using Emacs editing facilities, plus two
useful facilitis.

The mode defines also two useful functions to "submit" your JCL
to a running instance of MVS or Z/OS, provided that

1. Your MVS or z/OS has a "card reader" listening on a port
   (default 3505).
2. You have the necessary credentials to run the job on the OS.

The two functions (which are available on the Emacs menubar) are
`jcl-submit` (alias `submit`; Emacs and ELisp do not define this
as a function) and `jcl-submit-file` (a misnomer; it should be
"jcl-submit-dataset").  The two functions submit either the
current buffer or a file of your choice to the "card reader".

Files with `.jcl` extensions are opened in `jcl-mode`. `jcl-poly-mode'
can be entered by invoking the eponimous function.

Most behavior is customizable in the "jcl" group.

If you manually downloaded the package, you can open the
`testsubmit.jcl` file, which just executes `IEFBR14` and
allocates a dataset and submit it to your MVS or z/OS
instance.


## asmibm-mode

**asmibm-mode** is a major mode to handle Assembler/IBM MVS or z/OS
ASM IBM.  The mode just does highlighting and some minor indentation.

More functionalities may come in the future.


## pl1-mode

**pl1-mode** is a major mode to handle PL/I code.  The mode just does
highlighting and some minor indentation.  Syntax recognized is quite
limited, but most statements and attributes are properly highlighted.

More functionalities may come in the future.


# Installing and Using IRON MAIN

To use the package, just install the folder `iron-main` in your Emacs
setup and ensure that the file `iron-main.el` is loaded.  Files with
`.jcl` extension will now load in `jcl-mode`.


### A NOTE ON FORKING

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.
It helps keeping the entropy level at an acceptable level.



Enjoy

Marco Antoniotti, Milan, Italy, (c) 2020-2021
