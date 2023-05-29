echo "Y" | pacman -Syy >> mintty_output.txt
echo "Y" | pacman -Syu >> mintty_output.txt
echo "Y" | pacman -S wget >> mintty_output.txt
echo "Y" | pacman -S tar >> mintty_output.txt
echo "Y" | pacman -S unzip >> mintty_output.txt
echo "Y" | pacman -S zip >> mintty_output.txt
echo "Y" | pacman -S man >> mintty_output.txt
wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz >> mintty_output.txt
tar xvfvz autoconf-2.69.tar.gz >> mintty_output.txt
cd autoconf-2.69 >> ../mintty_output.txt
./configure >> ../mintty_output.txt
make >> ../mintty_output.txt
make install >> ../mintty_output.txt
sleep 120
