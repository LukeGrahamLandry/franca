set -e
target_name="$(uname -s)_$(uname -m)"
url=""
hash=""
case "$target_name" in
    Darwin_arm64)
        FRANCA_BACKTRACE=1 ./boot/franca-macos-arm64 tests/run_tests.fr -- github; exit 0;;
    Darwin_x86_64)
        url="bd44f7952dd8a61ad73a3e6796068ec60c890409/franca-macos-amd64";
        hash="791713ded192972525d3ceac291b65bcafb7ae5ca037b128d684dc5b56c8e095";
    ;;
    Linux_aarch64)
        url="bdc57d6698872d45747206316a6721fe358ddb79/franca-linux-arm64";
        hash="a9575b167fd87ed9c294402316515346ec6d3a353de07770078374ea976d9c9d";
    ;;
    Linux_x86_64)
        url="bd44f7952dd8a61ad73a3e6796068ec60c890409/franca-linux-amd64-dyn";
        hash="8d2a9da9e571a86911c06f09f543e7ba55a2cec641a18e3611cffb8931cd4f42";
    ;;
  *)
    echo "Unsupported target: ${target}"; exit 2;;
esac

mkdir -p target
exe_path="./target/${hash}.out"
if [ ! -x "$exe_path" ]; then
    # TODO: curling stuff is not acceptable
    curl "https://lukegrahamlandry.ca/franca/bin/${url}" -o "$exe_path"
    echo "$hash $exe_path" > ./target/${hash}.hash 
    sha256sum --check ./target/${hash}.hash
    chmod +x "$exe_path"
fi
FRANCA_BACKTRACE=1 "$exe_path" tests/run_tests.fr -- core
