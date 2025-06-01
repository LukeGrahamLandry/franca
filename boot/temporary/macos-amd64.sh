mkdir target
curl https://lukegrahamlandry.ca/franca/bin/8a6e31988d72e3d6b85f6822aa1a251822afdd7e/franca-macos-amd64 -o ./target/boot.out
chmod +x ./target/boot.out
./target/boot.out run_tests.fr -- core
