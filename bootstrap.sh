RUST_MIN_STACK=8388608 cargo test || {
  echo "Initial test failed."; exit 1;
}

cargo run -- bootstrap || {
  echo "Bootstrap failed. No changes made.";
  exit 1;
}

mv "src/experiments/bootstrap_gen.rs" "target/bootstrap_gen.rs.temp" || {
 echo "Save failed. No changes made.";
 exit 1;
}

mv "lib/codegen/aarch64/basic.gen.txt" "target/aarch64_basic.gen.txt.temp" || {
   echo "Save failed. Reverting other save.";
    mv "target/bootstrap_gen.rs.temp" "src/experiments/bootstrap_gen.rs"
   exit 1;
}

mv "target/bootstrap_gen.rs" "src/experiments/bootstrap_gen.rs"
mv "target/aarch64_basic.gen.txt" "lib/codegen/aarch64/basic.gen.txt"

RUST_MIN_STACK=8388608 cargo test || {
  echo "Bootstrapped test failed. Reverting.";
  mv "target/bootstrap_gen.rs.temp" "src/experiments/bootstrap_gen.rs"
  mv "target/aarch64_basic.gen.txt.temp" "lib/codegen/aarch64/basic.gen.txt"
  exit 1;
}

echo "Bootstrap Successful. Cleaning up. "
rm "target/bootstrap_gen.rs.temp"
rm "target/aarch64_basic.gen.txt.temp"


