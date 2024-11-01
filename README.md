<!-- ![ReFlow Logo](logo.png "ReFlow") -->

<img src="logo.png" width="300">

----------------------

ReFlow is a fully automatic floating-point code extractor.
Given a [PVS](https://pvs.csl.sri.com/) real-valued program and the target floating-point precision (single or double), ReFlow generates the corresponding floating-point C implementation.

ReFlow implements a code instrumentation to detect unstable conditionals (i.e., the floating-point computational flow diverges with respect to the ideal real number one) by over-approximating the conditional guards in the if-then-else statements.
The resulting transformed program emits a warning when an unstable test is detected.

The generated C code is annotated with [ACSL](https://frama-c.com/html/acsl.html) (ANSI/ISO C Specification Language
) contracts that relate the floating-point implementation with the real-valued program specification.
ReFlow relies on the static analyzer [PRECiSA](https://github.com/nasa/PRECiSA) to compute sound round-off error estimations and the corresponding PVS proof certificate that guarantee their correctness.

The annotated code can be input to the static analyzer [Frama-C](https://frama-c.com/).
The WP plug-in has been customized to support the PVS certificates generated by PRECiSA in the proof of correctness of the C program. ReFlow also provides a collection of PVS proof strategies that to help discharge the VCs generated by Frama-C.

ReFlow supports a wide variety of operators, conditionals, let-in expressions, non-recursive function calls, bounded for-loops, and predicates.

# Installation

ReFlow runs on Linux and Mac OS X operating systems.

## Prerequisites

To build and install ReFlow you need:
- The [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (version `>=8.10.7`) and
- its package manager [Cabal](https://cabal.readthedocs.io/en/3.6/) (version `>=3.6.2.1`).
- `Make`
- `wget` or `curl` HTTP clients
- `git`
- `CMake`
- *C99/C++17* compilers

Alternatively, a *Dockerfile* is provided to create a container with all the previous prerequisites that can be used to build ReFlow.

To analyze the generated C program you need:
- [Why3 1.4.1](https://why3.lri.fr/)
- [Frama-C 24.0 Chromium](https://frama-c.com/fc-versions/chromium.html)

## Build

A *Makefile* is provided to automate the downloading and building of *Kodiak* and its dependencies, and to configure and build ReFlow.

 The only command that needs to be run from the folder containing the `Makefile` file is:
```sh
$ make
```

If you want to install ReFlow inside a container, a *Dockerfile* is provided. Given a standard *Docker* installation and the *ReFlow* repository path `<ReFlow-path>`,
the commands to install *ReFlow* inside the container are:
```sh
$ cd <ReFlow-path>
$ git submodule update --init --recursive
$ cd deps/precisa
$ docker build -t precisa-container .
...
$ docker run -v <ReFlow-path>:/reflow  -it precisa-container /bin/bash
docker$ cd reflow
docker$ make
...
docker$ echo "Now reflow is available for the current session"
docker$ reflow
...
```

# Automatic generation of stable C code from PVS

The input to ReFlow C are the following files:

* A program `example.pvs` composed of real-valued functions. In its current version, ReFlow accepts a subset of the language of the Prototype Verification System (PVS), including LET expressions, IF-THEN-ELSE constructions, function calls, and floating point values and operations such as: addition, multiplication, division, subtraction, floor, square root, trigonometric functions, logarithm, and exponential. For example:
   ```
   example: THEORY
   BEGIN

	example (X,Y: real) : real =
		IF (X >= 0 AND Y < 4)
		THEN IF (Y > 0)
			 THEN X+Y
			 ELSE X*Y
			 ENDIF
        ELSE X-Y ENDIF

   END example
   ```

* A file `example.input` containing initial ranges for the input variables of the program. For example:
   ```
   example(X,Y): X in [-10,10], Y in [0, 40]
   ```

The output is a C floating-point program `example.c` instrumented to detect unstable tests.
This program is annotated with ACSL contracts stating the relation between the floating-point program and the real number specification. This annotated program can be analyzed with the [Frama-C](https://frama-c.com/) static analyzer.

Besides, PVS certificates are provided for ensuring that all the unstable tests are detected.
These certificates can be automatically checked as explained [here](#How-to-verify-the-PVS-certificates).


## How to run ReFlow

We assume that `reflow` (the executable of ReFlow) is in the current directory.

To launch the round-off error analysis of ReFlow with the default parameters run:
```
$ ./reflow "example.pvs" "example.input"
```

- the first parameter is the path to the PVS program to be analyzed;
- the second one is the path to the file that indicates the initial values for the input variables of the input program;

### Command Line Options

- `--format FORMAT` where FORMAT can be double or single, indicating the target format of the floating-point C code. The default value is double precision.

## How to compile the generated C code

In order to compile and analyze the generated code, you will need the file [precisa_prelude.c](share/c/precisa_prelude.c).
Copy this file to your working directory.

To analyze the generated code you can use the script [run-framac.sh](tools/run-framac.sh).
This script assumes
1) there is an environment variable `REFLOW_HOME` containing the path to the local copy of this directory and
2) the `frama-c` executable is available from the directory where you are running the script.

For example,
```shell
% export REFLOW_HOME="/path/to/reflow/no/trailing/slash"
% cd my_example
my_example% ls
my_generated_code.c
my_example% cp $REFLOW_HOME/share/c/precisa_prelude.c .
my_example% $REFLOW_HOME/tools/run-framac.sh my_generated_code.c
```

# Information

## Version

*ReFlow v-1.0.0* (December 2023)

## Contact information
If you have any question or problem, please contact:

* [Laura Titolo](mailto:laura.titolo@nasa.gov) (for ReFlow)
* [Mariano Moscato](mailto:mariano.m.moscato@nasa.gov) (for PVS and Frama-C support)
* [Marco A. Feliu](mailto:marco.feliu@nasa.gov) (for Kodiak and installation issues)
* [César Muñoz](mailto:cesar.a.munoz@nasa.gov) (for ReFlow at NASA)

## Additional Contributors

* Nikson Bernardes Fernandes Ferreira, University of Brasilia
* Caleb Chan, University of Washington

## Related Publications

- Nikson Bernardes Fernandes Ferreira, Marco A. Feliú, Laura Titolo, Mauricio Ayala-Rincon:
  A provably correct floating-point implementation of Well Clear Avionics Concepts. FMCAD 2023

- 	Laura Titolo, Mariano M. Moscato, Marco A. Feliú, César A. Muñoz:
   Automatic Generation of Guard-Stable Floating-Point Code. IFM 2020

- Mariano Moscato, Laura Titolo, Marco A. Feliú and César Muñoz:
  A Provably Correct Floating-Point Implementation of a Point-in-Polygon Algorithm. FM 2019

- Laura Titolo, César A. Muñoz, Marco A. Feliú, Mariano M. Moscato:
  Eliminating Unstable Tests in Floating-Point Programs. LOPSTR 2018


## License and Copyright Notice

The code in this repository is released under NASA's Open Source Agreement.  See the directory [`licenses`](licenses).

<pre>
Notices:
Copyright 2023 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

This software calls the NASA Software named “PRECiSA with Instrumented Code Generation” LAR-19739-1, which is not bundled or redistributed with this software, but users of this software must obtain their own NASA license, which is subject to the terms and conditions of the applicable at the time.

Disclaimers
No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
</pre>