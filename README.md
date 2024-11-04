1. configure the bglstone suite:

   ./configure --bigloo=$HOME/prgm/project/bigloo/bigloo/bin/bigloo
   
If you are to test another Bigloo version:

   ./configure --bigloo=$HOME/prgm/project/bigloo/bigloo/bin/bigloo


2. To test the benchmark harness

   make compile TARGETS="bigloo" BENCH=test
   
   
3. compile the various benchmarks:

   make compile TARGETS="bigloo bigloo-saw bigloo-jvm bigloo-wasm" BENCH=fullbgl
   
To compile only the _regular_ Scheme benchmarks:
   make compile TARGETS="bigloo bigloo-jvm bigloo-wasm" BENCH=scm

If you are only interested by the C backend use:

   make compile


4. run the benchmarks:

  make run TARGETS="bigloo bigloo-jvm bigloo-wasm" BENCH=fullbgl


5. generate a plot

  bin/gnuplothistogram --relative src/bigloo.stat src/bigloo-saw.stat src/bigloo-jvm.stat src/bigloo-wasm.stat
  gnuplot fig.plot
  okular fig.pdf
  

6. Misc

To test bencharks correctness:
  
  make run TARGETS="bigloo bigloo-saw bigloo-jvm bigloo-wasm" BENCH=fullbgl REPETITION="-r 1"


