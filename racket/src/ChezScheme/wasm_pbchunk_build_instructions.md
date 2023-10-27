# Wasm-PBChunk Build Instructions

## Getting a proper build of Chez Scheme
To use wasm-pbchunk, it is currently necessary to have a build of Chez Scheme which targets pb64l (64-bit, little endian). 

This can be done from the Chez Scheme root `racket/racket/src/ChezScheme` with the following build configuration:

The first thing to do is to generate PB boot files. This will allows for a bootstrapping build.
This can be done with an existing installation of racket, using:

`$ racket ../rktboot/main.rkt --machine pb64l`

Then,

`$ ./configure --pbarch --nothreads`
(should output "Configuring for pb64l")

`$ make`

`$ make install`

This should yield a build of `scheme` which targets `pb64l`.

## Building a Scheme boot file for use with wasm-pbchunk
With the proper `scheme` build in place, we can now build a scheme boot file for use with testing.

For example, suppose we have the following scheme file:

```scheme
;; fact.scm
(define (fact n)
    (cond 
        [(equal? n 0) 1]
        [(* n (fact (- n 1)))]))

(scheme-start (lambda ()
    (printf "(fact 10): ~a" (fact 10))))
```

Note that `scheme-start` is a parameter which sets the code which should be invoked by Chez Scheme when it starts (normally, it would be the REPL). This parameter must be set to execute a standalone program in a boot file. The REPL functionality does not work very well currently under Wasm; when executing under node, reading input lines seems to fail. When executing in-browser, there is an inconvenient browser "input" prompt for each REPL iteration and it is hard to see the output.

To compile `fact.scm` to an **object** file, the `(compile-file ...)` form can be used. For example, 

`$ echo "(compile-file \"fact.scm\") |" scheme` (produces `fact.so`)

Once you have the object file, we can construct a boot file with `(make-boot-file ... )`. For example:

`$ echo "(make-boot-file \"fact.boot\" (list \"petite\") \"fact.so\")" | scheme`

This creates the boot file `fact.boot` from `fact.so`, which depends on `petite.boot` (a smaller version of the core Chez Scheme boot files)

## Chunking
To run wasm-pbchunk, we must invoke the `wasm-bootpbchunk` script on our boot file. 
From the *root* of Chez Scheme, run:

`$ scheme --script s/wasm-bootpbchunk.ss --dest pbchunk-out --arch pb64l --extra-boot fact.boot --only-funcs fact`

The `--only-funcs` parameter allows for chunking only the listed functions. Alternatively, the `--exclude-funcs` parameter can be used to exclude certain functions while chunking every other function. This is useful if there are functions which are known to have unimplemented/incompatible instructions, or to avoid chunking a `main` function that performs testing, for example.

In this case, the emitted wasm module will be under the `pbchunk-out` directory.

## Building For Wasm
To build a version of Scheme for Wasm which will use the provided chunked boot file, we have to re-configure in place with a *kernel only* configuration. A *kernel only* configuration only affects how the Chez Scheme kernel (C and asm implemented portions) is compiled. To do this, we can run:

`./configure --emscripten --pbarch --nothreads --emboot=pbchunk-out/fact.boot`

The main thing to note here is the `--emboot` parameter, which sets an additional boot file to load (in addition to the standard Chez Scheme boot files). In this case, we want to use the modified boot file output by wasm-pbchunk, which will be `pbchunk-out/fact.boot`. 

Now, we can run

`$ make`

The build output will be under `em-pb64l/bin/`. The compiled Wasm file will be under 
`em-pb64l/bin/pb64l/scheme.wasm`. Emscripten will also generate a `scheme.js` file. 

To run, first `cd em-pb64l/pb64l/bin/`, then invoke `node scheme.js`. Changing directories is necessary, because the `scheme.js` script will load the wasm module, and expects it to be in its working directory. It may be possible to change node's working directory with an environment variable, but it was unclear how to do this.

## Running the benchmarks
The Larceny R6RS benchmarks are located under `ChezScheme/R6RS`. This is a copy of the following [folder](https://github.com/larcenists/larceny/tree/master/test/Benchmarking/R6RS).

### Dependencies
An existing pb64l Chez Scheme scheme build is needed in order to execute the benchmarks, so do this first (following the preceding steps).

The benchmarking script uses `wasm-opt` manually, so [binaryen](https://github.com/WebAssembly/binaryen) needs to be installed and `wasm-opt` needs to be in the shell path.

### Steps

From `ChezScheme`, 

`$ cd R6RS`

`$ ./bench wasmpbchunk <benchmark or benchmark group>`
`
The `bench` script is provided by the larceny project, but has been modified with a wasmpbchunk build configuration option. 

To run the gabriel benchmarks (used for the thesis), you would use:

`$ ./bench wasmpbchunk gabriel`

### How the script works

When running benchmarks for wasm-pbchunk, the script basically follows the steps described previously, for each individual benchmark; the script first compiles a benchmark to a pb64l boot file, then `cd`s up a directory into chez scheme and re-configures chez scheme in-place to use that boot file (using the `--emboot` configure option), before rebuilding chez with `make`. The resulting `scheme.wasm` is  executed using node and the whole node execution is timed.

The stdout of the benchmark script is stored under `results.<platform>`, in this case `platform` is `chez_wasmpbchunk`.

## High-level overview

The system consists of the following files:
- `wasm-bootpbchunk.ss`
    The main script for running pbchunk on a boot file. Usage has already been described
- `wasm-pbchunk.ss`
    Similar to `pbchunk.ss` -- many parts are duplicated. Provides the boilerplate for extracting code objects from a boot file and chunking said code objects. Some core functions to look at are 
    - `gather-targets` which determines locations of labels, relocations, and rp-headers (this code is also used in the PB disassembler), 
    - `select-instruction-range` which implements the chunk selection procedure to select a next chunk, and `compile-chunklet`, which does the actual translation.
    - `compile-chunklet` compiles an actual chunklet into Wasm instructions
- `wasm-emit.ss`
    Contains the code to translate pb instructions into wasm. Most translations are fairly direct, with the one exception being the creation of temporary locals which are hoisted

## Some other notes
It is useful to have copies of `racket` and `raco` for PB. This way, the raco disassemble tool can be used.
Unfortunately, it has proven difficult to build racket for `pb64l`, so it will probably have to be a `tpb64l` build. This means that it is necessary to build two copies of a program which is being used with wasm-pbchunk:
one which uses `pb64l`, and the other which uses `tpb64l`. 

Running `bin/zuo` <arch>/s allows for rebuilding only the portions of Chez Scheme which have changed. 
For example, `bin/zuo pb64l/s` will re-compile only the scheme source associated with object files which are out of date. This is useful when working on wasm-pbchunk, since the feature only touches ~3 files in the compiler.

Don't be afraid to re-configure the build in place. This has to be done CONSTANTLY when working with wasm-pbchunk.

## Brief Note: Why is everything designed this way?
Wasm-pbchunk is very similar to the original pbchunk, and has a fair bit of shared, duplicated code. During development, the most straightforward option by far was to duplicate the existing pbchunk code and modify it for use with Wasm. This led to some quirks, such as the fact that wasm-pbchunk only works on boot files for the time being. This does necessarily mean that the general approach can't be used on normal object files or code in-memory -- this is just the starting point to make things simple. 

It remains to be seen whether Wasm-pbchunk will depart more from the original pb-chunk, whether it could be made interactive, etc. I am hoping there is a design for this system that can remove some of the major limitations that are currently in place (i.e., needing to use boot files, and having to re-build Chez Scheme for every program)
