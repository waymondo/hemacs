printf " Installing Emacs"
/usr/bin/env brew install emacs --with-cocoa --HEAD
/usr/bin/env brew linkapps emacs
printf " Cloning Hemacs"
/usr/bin/env git clone https://github.com/waymondo/hemacs.git "$HOME/.emacs.d"
printf " Installing Hemacs"
cd "$HOME/.emacs.d"
read -p " Install Extra Dependencies? " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
  /usr/bin/env brew install git terminal-notifier node the_silver_searcher bash-completion coreutils trash pandoc
  /usr/bin/env npm install -g js-beautifier marked less coffee-script js2coffee eslint babel-eslint tern
  /usr/bin/env gem install gem-ripper-tags rubocop ruby-lint
fi
printf " Done"
