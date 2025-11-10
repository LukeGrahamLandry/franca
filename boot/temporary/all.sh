set -e
make
franca examples/os/build.fr -qemu
./boot/temporary/macos-amd64.sh
franca tests/exe/wasm.fr
orb -m arm64 ./boot/temporary/linux-arm64.sh
orb -m amd64 ./boot/temporary/linux-amd64.sh
orb -m arm2 ./target/release/franca-linux-rv64 run_tests.fr -- core
