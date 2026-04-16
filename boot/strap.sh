set -e
target_name="$(uname -s)_$(uname -m)"
url=""
hash=""
boot_commit="2fa04a7374f3202c9778d6b9b70701072beb87f6"
case "$target_name" in
    Darwin_arm64)
        url="franca-macos-arm64";
        hash="b13e4b1abd5a53ede47e83f9aed0dbeeab40693c49b039d41efce3edbebabf31";
    ;;
    Darwin_x86_64)
        url="franca-macos-amd64";
        hash="1e2b82d9bd91979978f4e51e656f5b1347a241751cff231724fd474b8b8bfa86";
    ;;
    Linux_aarch64)
        url="franca-linux-arm64-sta";
        hash="753c70964376ee8ce2957fa842053b3a351130795be56115098fd2d73fbafca6";
    ;;
    Linux_x86_64)
        url="franca-linux-amd64-sta";
        hash="08ac2bb1282fc2c90943b6677699897dc7a9447853a1a933f2d125c7d442a347";
    ;;
    Linux_riscv64)
        url="franca-linux-rv64-sta";
        hash="8497ea9e03c323c8fe206569084a0e0b0c63880596e970e82ccb8b6151d7f2f4";
    ;;
  *)
    echo "Unsupported target: ${target}"; exit 2;;
esac

mkdir -p target/franca/fetch
exe_path="./target/franca/fetch/${hash}.out"
if [ ! -x "$exe_path" ]; then
    curl "https://lukegrahamlandry.ca/franca/bin/${boot_commit}/${url}" -o "$exe_path.check"
    echo "$hash  $exe_path.check" > ./target/franca/fetch/${hash}.hash 
    shasum -a 256 --check ./target/franca/fetch/${hash}.hash
    mv "$exe_path.check" "$exe_path"
    chmod +x "$exe_path"
fi


# [[ "$*" == *"-no-test"* ]] but i want it to not need bash
notest=""
for it in "$@"; do
  if [ "$it" = "-no-test" ]; then
    notest="true"
    break
  fi
done

if [ ! -z "$notest" ]
then
    FRANCA_BACKTRACE=1 "$exe_path" tests/run_tests.fr strap
    ./target/f.out tests/run_tests.fr release
else
    FRANCA_BACKTRACE=1 "$exe_path" tests/run_tests.fr -- core
fi
