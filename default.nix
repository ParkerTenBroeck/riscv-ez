{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  rustupToolchain = "nightly";

  rustBuildTargetTripple = "riscv32gc-unknown-linux-gnu";
  rustBuildHostTripple = "x86_64-unknown-linux-gnu";
  riscv-cross = import pkgs.path {
    crossSystem = {
      config = "riscv64-none-elf";
    };
  };

  riscv = riscv-cross.stdenv.cc;

in
#let
#  mkShell = pkgs.mkShell.override { stdenv = pkgs.stdenvAdapters.useMoldLinker pkgs.stdenv; };
#in mkShell

 pkgs.mkShell  rec {

    nativeBuildInputs = with xorg; [
        libxcb
        libXcursor
        libXrandr
        libXi
        pkg-config
    ] ++ [
      python3
      libGL
      libGLU
    ];

    buildInputs = with pkgs; [
      # Replace llvmPackages with llvmPackages_X, where X is the latest LLVM version (at the time of writing, 16)
      llvmPackages.bintools
      rustup

      riscv
      
        libGL
        libxkbcommon
        wayland
        xorg.libX11
        xorg.libXcursor
        xorg.libXi
        xorg.libXrandr
    ];


    RUSTC_VERSION = "nightly";
    # https://github.com/rust-lang/rust-bindgen#environment-variables
    LIBCLANG_PATH = pkgs.lib.makeLibraryPath [ pkgs.llvmPackages_latest.libclang.lib ];
    shellHook = ''
      export PATH=$PATH:''${CARGO_HOME:-~/.cargo}/bin
      export PATH=$PATH:''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin/
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/opengl-driver/lib/:${pkgs.lib.makeLibraryPath [
        pkgs.alsa-lib
        pkgs.udev
        pkgs.vulkan-loader
        
                libGL
        libxkbcommon
        wayland
        xorg.libX11
        xorg.libXcursor
        xorg.libXi
        xorg.libXrandr
      ]}"
      '';
    # Add precompiled library to rustc search path
    RUSTFLAGS = (builtins.map (a: ''-L ${a}/lib'') [
      # add libraries here (e.g. pkgs.libvmi)
    ]);
    # Add glibc, clang, glib and other headers to bindgen search path
    BINDGEN_EXTRA_CLANG_ARGS = 
    # Includes with normal include path
    (builtins.map (a: ''-I"${a}/include"'') [
      # add dev libraries here (e.g. pkgs.libvmi.dev)
      pkgs.glibc.dev 
    ])
    # Includes with special directory paths
    ++ [
      ''-I"${pkgs.llvmPackages_latest.libclang.lib}/lib/clang/${pkgs.llvmPackages_latest.libclang.version}/include"''
      ''-I"${pkgs.glib.dev}/include/glib-2.0"''
      ''-I${pkgs.glib.out}/lib/glib-2.0/include/''
    ];

  }
