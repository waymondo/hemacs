An ever evolving Carbon Emacs 24.4+ setup on OSX. I'm mainly writing Ruby and CoffeeScript, interfacing with Git and Bash, writing compiled stylesheets and templates, and hacking on this Elisp dump.

### Installation

Install [Homebrew](http://mxcl.github.com/homebrew) and [Cask](http://cask.readthedocs.org/en/latest/guide/installation.html), then:

```
brew install emacs --cocoa --srgb --with-glib --with-imagemagick --use-git-head --HEAD
brew linkapps
git clone git://github.com/waymondo/hemacs ~/.emacs.d && cd ~/.emacs.d 
cask
open /Applications/Emacs.app
```

##### Extra Dependencies

```
brew install git hub ag terminal-notifier node the_silver_searcher ctags markdown bash-completion xz coreutils
# follow brew post-install instructions
npm install -g js-beautifier
```

Uses [Meslo LG M DZ for Powerline](https://github.com/Lokaltog/powerline-fonts) as the default font.

Control Google Chrome with the [https://github.com/puffnfresh/crab-chrome](Crab Chrome Extension).
