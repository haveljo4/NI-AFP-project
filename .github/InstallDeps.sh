echo "before -Syy" >> mintty_output.txt
yes | pacman -Syy >> mintty_output.txt
echo "after -Syy" >> mintty_output.txt
yes | pacman -Syu >> mintty_output.txt
yes | pacman -S wget >> mintty_output.txt
yes | pacman -S tar >> mintty_output.txt
yes | pacman -S unzip >> mintty_output.txt
yes | pacman -S zip >> mintty_output.txt
yes | pacman -S man >> mintty_output.txt
echo "after pacman" >> mintty_output.txt
yes | wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz >> mintty_output.txt
yes | tar xvfvz autoconf-2.69.tar.gz >> mintty_output.txt
yes | cd autoconf-2.69 >> ../mintty_output.txt
echo "running configure" >> mintty_output.txt
yes | ./configure >> ../mintty_output.txt
yes | make >> ../mintty_output.txt
echo "running make" >> mintty_output.txt
yes | make install >> ../mintty_output.txt
sleep 10
exit
