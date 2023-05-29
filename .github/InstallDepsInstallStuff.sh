echo "pacman -Syy --noconfirm &>> mintty_output.txt ;" &>> mintty_output.txt
pacman -Syy --noconfirm &>> mintty_output.txt
echo "before installing stuff with pacman " &>> mintty_output.txt
pacman -S --noconfirm git wget tar man gzip automake make libtool patch unzip xz bison flex pkg-config &>> mintty_output.txt
echo "after installing stuff with pacman " &>> mintty_output.txt
wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
tar xvfvz autoconf-2.69.tar.gz  &>> ../mintty_output.txt
cd autoconf-2.69 &>> ../mintty_output.txt
./configure &>> ../mintty_output.txt
make &>> ../mintty_output.txt
make install &>> ../mintty_output.txt
exit


