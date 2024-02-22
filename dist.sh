RUST_MIN_STACK=8388608 cargo test || exit
cargo build --target wasm32-unknown-unknown --release --lib --no-default-features --features some_log || exit

TARGET_PATH="target/wasm32-unknown-unknown/release"
WASM_PATH="$TARGET_PATH/franca.wasm"
WEB_DIST_PATH="../LukeGrahamLandry.github.io/franca"
mkdir -p "$WEB_DIST_PATH/$TARGET_PATH"
cp "$WASM_PATH" "$WEB_DIST_PATH/$WASM_PATH"
cp 'index.html' "$WEB_DIST_PATH/index.html"
cp -r "tests" "$WEB_DIST_PATH"
