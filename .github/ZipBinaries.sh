mkdir ./tmp  >> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libstdc++-6.dll ./tmp  >> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libgcc_s_seh-1.dll ./tmp  >> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libwinpthread-1.dll ./tmp  >> mintty_output.txt
zip tmp.zip ./tmp/* >> mintty_output.txt