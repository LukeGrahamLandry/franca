# THIS IS UNFINISHED
# TODO: don't skip the external tests that need libc headers (use $NIX_CFLAGS_COMPILE?)
# TODO: always enable the wasm tests in node
# TODO: add a sr.ht build file that uses this
# TODO: support darwin. does that need the same loader path hacks?
# TODO: do the flake-utils thing so you don't have to edit the system variable 
#       then magic words: nix develop ./tests#devShell.x86_64-linux
# TODO: have buildphase do '[run_tests] release' so it can build for all the arches?
# TODO: add wast2json to dependencies for tests/external/wasm_spec.fr
# TODO: maybe i want to fix some stuff before committing to the new bootstrap version being 2fa04a7374f3202c9778d6b9b70701072beb87f6 so i might break this flake

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };
  outputs = { self, nixpkgs }:
      let
            system = "aarch64-linux";
            # system = "x86_64-linux";
            pkgs = import nixpkgs { inherit system; };
            dependencies =
            (with pkgs; [
                git
                curl  # for tests/(external, deps.fr) (to check import_c works)
                clang  # for tests/backend.fr -use_linker (to check .o files are valid), also examples/emit_c.fr 
                nodejs  # for tests/test.js (to check wasm modules are valid)
                # TODO: python3+openssl is an insanely heavy dependency just for usefuckinghttpssoicandrawonthefuckingscreen.py
            ]);
          
            franca_boot_bins = {
                "aarch64-linux" = pkgs.fetchurl {
                    url = "https://lukegrahamlandry.ca/franca/bin/2fa04a7374f3202c9778d6b9b70701072beb87f6/franca-linux-arm64-sta";
                    hash = "sha256-dTxwlkN27ozilX+oQgU7OjURMHlb5WEVCY/S1z+6/KY=";
                };
                "x86_64-linux" = pkgs.fetchurl {
                    url = "https://lukegrahamlandry.ca/franca/bin/2fa04a7374f3202c9778d6b9b70701072beb87f6/franca-linux-amd64-sta";
                    hash = "sha256-CKwrsSgvwskJQ7ZndpmJfcepRHhToakz8tElx9RCo0c=";
                };
            };
            franca_boot = franca_boot_bins.${system};
      in
      {
        franca = pkgs.stdenv.mkDerivation {
            name = "franca";
            src = ./..;
            buildPhase = ''
                mkdir -p "$out"
                franca="$out/franca.${system}.out"
                cp -f ${franca_boot} "$franca"1
                chmod +x "$franca"1
                export FRANCA_NO_CACHE=1
                # the one in franca_boot_bins doesn't support changing this so rebuild an extra time first
                export FRANCA_ELF_INTERP_PATH="$(cat $NIX_CC/nix-support/dynamic-linker)"
                "$franca"1 examples/default_driver.fr build compiler/main.fr -unsafe -o "$franca"2 -syscalls
                "$franca"2 examples/default_driver.fr build compiler/main.fr -unsafe -o "$franca"
                echo $out
            '';
            installPhase = ''
                echo ""  # don't try to use my makefile
            '';
        };
        devShell.${system} = pkgs.mkShell {
          buildInputs = [dependencies];
          shellHook = ''
             # TODO: how do i make this part live in the beginning so it doesn't rerun every time you enter the shell?
             mkdir -p target
             franca="${self.franca.out}/franca.${system}.out"
             export FRANCA_ELF_INTERP_PATH="$(cat $NIX_CC/nix-support/dynamic-linker)"
             alias franca="$franca" 
             function tests() {
                "$franca" tests/run_tests.fr run
             }
             echo "tests() :: 'franca tests/run_tests.fr run'"
          '';
        };
      };
}
