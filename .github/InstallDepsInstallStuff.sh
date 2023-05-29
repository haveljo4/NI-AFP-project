echo "pacman -Syy --noconfirm &>> mintty_output.txt ;" &>> mintty_output.txt
pacman -Syy --noconfirm &>> mintty_output.txt ;
echo "before installing stuff with pacman " &>> mintty_output.txt
pacman -S --noconfirm git wget tar man gzip autoconf automake make libtool patch unzip xz bison flex pkg-config &>> mintty_output.txt ;
echo "after installing stuff with pacman " &>> mintty_output.txt
exit


