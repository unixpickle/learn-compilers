tmp_dir=./benchmark_state
mkdir -p $tmp_dir

llvm_dir=/opt/homebrew/opt/llvm/bin

compile() {
  swift run Compile $1 $2 2>/dev/null
}

run_benchmark() {
  code_dir=$1
  input_file=$2

  echo "Benchmarking $code_dir ..."

  echo 'C with clang:'
  clang -O3 $code_dir/ref.c -o $tmp_dir/a.out
  cat $input_file | time $tmp_dir/a.out >/dev/null

  echo 'C?? with native backend:'
  compile $code_dir/code $tmp_dir/out.s
  clang $tmp_dir/out.s -o $tmp_dir/a.out
  cat $input_file | time $tmp_dir/a.out >/dev/null

  echo 'C?? with LLVM backend:'
  compile $code_dir/code $tmp_dir/out.ll
  $llvm_dir/opt $tmp_dir/out.ll -O3 -o $tmp_dir/out.bc 2>/dev/null
  $llvm_dir/llc $tmp_dir/out.bc -o $tmp_dir/out.s 2>/dev/null
  clang $tmp_dir/out.s -o $tmp_dir/a.out 2>/dev/null
  cat $input_file | time $tmp_dir/a.out >/dev/null
}

echo $((64581383*479001599)) >$tmp_dir/factorize
run_benchmark examples/factorize $tmp_dir/factorize
run_benchmark examples/prime_sieve /dev/null
run_benchmark examples/brainf_ck examples/brainf_ck/mandelbrot.bf
