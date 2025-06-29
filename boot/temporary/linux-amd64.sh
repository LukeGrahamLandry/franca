mkdir target
curl https://lukegrahamlandry.ca/franca/bin/bd44f7952dd8a61ad73a3e6796068ec60c890409/franca-linux-amd64-dyn -o ./target/boot-linux-amd64-dyn.out
chmod +x ./target/boot-linux-amd64-dyn.out
./target/boot-linux-amd64-dyn.out run_tests.fr -- core
