mkdir target
curl https://lukegrahamlandry.ca/franca/bin/fb35471e34c5b707341542f1a8176219e80fbc23/franca-macos-amd64 -o ./target/boot-macos-amd64.out
chmod +x ./target/boot-macos-amd64.out
./target/boot-macos-amd64.out run_tests.fr -- core
