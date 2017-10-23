# Macro Benchmarks

This directory contains a framework for systematically
building, running, analyising, and archiving benchmarks
which measure the performance of grift programs as a whole.

## Getting Started

To build, run (and time), analyze, and archive all the
macro benchmarks simply run ```make```. This will
create a time stamped copy of the benchmark sources,
analysis output, and log files for the entire process.

There is no reason to make clean because a new copy
of the benchmark library is created each time make
is invoked.

## Important Analysis

## Testing the benchmarks

Running ``make test`` will run unit tests that should
execute quickly and imply that running a full make
will succeed without error.

## Organization / Reasoning

The entire benchmark suite is kept in single directory
with a driver makefile wrapped around it for archival
purposes. The make files purposefully do not have a make
clean feature to avoid deleting results. Instead running
the framework copies the ```src``` directory to a whose
name is a time stamp and runs make inside of this file.

Finer grained manipulation of the benchmark suite requires
more knowlege of the setup of the test suite than is exposed
at this level and should be within the src directory.

## Further Information

```src/README.md``` contains more information on the organization
of the building, running, and analysis of the benchmarks and how
to extend the benchmarking script.
