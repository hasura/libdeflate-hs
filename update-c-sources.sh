#! /bin/bash
set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")"

rm -r cbits/*

cd cbits
curl -L https://github.com/ebiggers/libdeflate/tarball/master | tar -xz --strip-components=1 
rm -r Makefile Makefile.msc programs scripts libdeflate.pc.in .g* .cirrus.yml

echo "------ Make sure to update C sources in cabal file: -------"
cd .. && ls cbits/lib/*.c cbits/lib/*/*.c
