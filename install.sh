printf " Installing Emacs"
/usr/bin/env brew install emacs --cocoa --srgb --with-glib --with-imagemagick --with-rsvg --HEAD
/usr/bin/env brew linkapps emacs
printf " Cloning Hemacs"
/usr/bin/env git clone https://github.com/waymondo/hemacs.git "$HOME/.emacs.d"
printf " Installing Hemacs"
