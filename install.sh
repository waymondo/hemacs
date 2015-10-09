printf " Installing Emacs"
/usr/bin/env brew install emacs --with-cocoa --HEAD
/usr/bin/env brew linkapps emacs
printf " Cloning Hemacs"
/usr/bin/env git clone https://github.com/waymondo/hemacs.git "$HOME/.emacs.d"
printf " Installing Hemacs"
/usr/bin/env cd "$HOME/.emacs.d"
/usr/bin/env git submodule update --recursive
