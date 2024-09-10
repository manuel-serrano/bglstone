1. configure the bglstone suite:

   ./configure --bigloo=$HOME/prgm/project/bigloo/bigloo/bin/bigloo
   
If you are to test another Bigloo version:

   ./configure --bigloo=$HOME/prgm/project/bigloo/bigloo/bin/bigloo


2. To test the benchmark harness

   make compile TARGETS="bigloo" BENCH=test
   
   
3. compile the various benchmarks:

   make compile TARGETS="bigloo bigloo-saw bigloo-jvm bigloo-wasm" BENCH=fullbgl
   
To compile only the _regular_ Scheme benchmarks:   
   make compile TARGETS="bigloo bigloo-saw bigloo-jvm bigloo-wasm" BENCH=scm

If you are only interested by the C backend use:

   make compile


4. run the benchmarks:

   BIGLOOJAVAOPT=-Xss8m BIGLOOWASMOPT=--stack-size=8192 make run TARGETS="bigloo bigloo-saw bigloo-jvm bigloo-wasm" BENCH=fullbgl


5. Misc

To test bencharks correctness:

   BIGLOOJAVAOPT=-Xss8m BIGLOOWASMOPT=--stack-size=8192 make run TARGETS="bigloo bigloo-saw bigloo-jvm bigloo-wasm" BENCH=fullbgl REPETITION="-r 1"


