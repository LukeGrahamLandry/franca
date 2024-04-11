RUST_BACKTRACE=1 cargo run || {
  echo "Initial test failed."; exit 1;
}

cargo run -- bootstrap || {
  echo "Bootstrap failed. No changes made.";
  exit 1;
}

mv "crates/compiler/src/bootstrap_gen.rs" "target/bootstrap_gen.rs.temp" || {
 echo "Save failed. No changes made.";
 exit 1;
}

mv "lib/codegen/aarch64/basic.gen.fr" "target/aarch64_basic.gen.fr.temp" || {
   echo "Save failed. Reverting other save.";
   mv "target/bootstrap_gen.rs.temp" "crates/compiler/src/bootstrap_gen.rs"
   exit 1;
}

mv "target/bootstrap_gen.rs" "crates/compiler/src/bootstrap_gen.rs"
mv "target/aarch64_basic.gen.fr" "lib/codegen/aarch64/basic.gen.fr"

RUST_BACKTRACE=1 cargo run || {
  echo "Bootstrapped test failed. Reverting.";
  echo "Note: cargo might incorrectly cache the tests so edit a random file before panicking."

  diff "target/bootstrap_gen.rs.temp" "crates/compiler/src/bootstrap_gen.rs" &> "target/bootstrap_gen.rs.diff"
  diff "target/aarch64_basic.gen.fr.temp" "lib/codegen/aarch64/basic.gen.fr" &> "target/aarch64_basic.gen.fr.diff"
  mv "crates/compiler/src/bootstrap_gen.rs" "target/bootstrap_gen.rs.failed"
  mv "lib/codegen/aarch64/basic.gen.fr" "target/aarch64_basic.gen.fr.failed"

  mv "target/bootstrap_gen.rs.temp" "crates/compiler/src/bootstrap_gen.rs"
  mv "target/aarch64_basic.gen.fr.temp" "lib/codegen/aarch64/basic.gen.fr"
  exit 1;
}

echo "Bootstrap Successful. Cleaning up. "
rm "target/bootstrap_gen.rs.temp"
rm "target/aarch64_basic.gen.fr.temp"


