mkdir target
curl https://lukegrahamlandry.ca/franca/bin/bd44f7952dd8a61ad73a3e6796068ec60c890409/franca-macos-amd64 -o ./target/boot-macos-amd64.out
chmod +x ./target/boot-macos-amd64.out
./target/boot-macos-amd64.out run_tests.fr -- core
