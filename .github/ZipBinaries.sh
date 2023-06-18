mkdir ./chess-tool-exe  >> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libstdc++-6.dll ./chess-tool-exe  &>> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libgcc_s_seh-1.dll ./chess-tool-exe  &>> mintty_output.txt
cp `dirname $(which ghc)`/../mingw/bin/libwinpthread-1.dll ./chess-tool-exe  &>> mintty_output.txt
cp -r ./.stack-work/dist/*/build/Chess-tool-exe/Chess-tool-exe.exe ./chess-tool-exe  &>> mintty_output.txt
cp ./resources/icon.png ./chess-tool-exe/icon.png  &>> mintty_output.txt
cp ./LICENSE ./chess-tool-exe/LICENSE  &>> mintty_output.txt
cp ./README.pdf ./chess-tool-exe/README.pdf  &>> mintty_output.txt
openssl genpkey -algorithm RSA -out private.key > /dev/null 2>&1
openssl x509 -req -days 365 -in csr.csr -signkey private.key -out certificate.pem  &>> mintty_output.txt
osslsigncode sign -certs certificate.pem -key private.key -in ./chess-tool-exe/Chess-tool-exe.exe -out ./chess-tool-exe/Chess-tool-exe-signed.exe &>> mintty_output.txt
rm private.key &>> mintty_output.txt
rm ./chess-tool-exe/Chess-tool-exe.exe  &>> mintty_output.txt
mv ./chess-tool-exe/Chess-tool-exe-signed.exe ./chess-tool-exe/Chess-tool-exe.exe  &>> mintty_output.txt
zip chess-tool-exe.zip ./chess-tool-exe/* &>> mintty_output.txt
exit