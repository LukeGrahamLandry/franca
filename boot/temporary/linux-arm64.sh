mkdir target
curl https://lukegrahamlandry.ca/franca/bin/bdc57d6698872d45747206316a6721fe358ddb79/franca-linux-arm64 -o ./target/boot-linux-arm64.out
chmod +x ./target/boot-linux-arm64.out
./target/boot-linux-arm64.out run_tests.fr -- core
