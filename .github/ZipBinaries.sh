mkdir ./chess-tool-exe  >> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libstdc++-6.dll ./chess-tool-exe  &>> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libgcc_s_seh-1.dll ./chess-tool-exe  &>> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libwinpthread-1.dll ./chess-tool-exe  &>> mintty_output.txt
cp -r ./.stack-work/dist/*/build/Chess-tool-exe/Chess-tool-exe.exe ./chess-tool-exe  &>> mintty_output.txt
zip chess-tool-exe.zip ./chess-tool-exe/* &>> mintty_output.txt
exit