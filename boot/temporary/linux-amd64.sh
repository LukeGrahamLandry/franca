mkdir target
curl https://lukegrahamlandry.ca/franca/bin/fb35471e34c5b707341542f1a8176219e80fbc23/franca-linux-amd64-dyn -o ./target/boot-linux-amd64-dyn.out
chmod +x ./target/boot-linux-amd64-dyn.out
./target/boot-linux-amd64-dyn.out run_tests.fr -- core
