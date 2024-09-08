1. configure the bglstone suite:

   ./configure

2. compile the various benchmarks:

   make compile TARGETS="bigloo bigloo-jvm bigloo-wasm" BENCH=fullbgl

If you are only interested by the C backend use:

   make compile


3. run the benchmarks:

   make run TARGETS="bigloo bigloo-jvm bigloo-wasm" BENCH=fullbgl

