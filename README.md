## Hemacs

One man's ever evolving Carbon Emacs 24.4 setup on OSX. I'm mainly writing Ruby and CoffeeScript, interfacing with Git and Bash, writing compiled stylesheets and templates, and hacking on this elisp dump.

### Installation

You'll need [Homebrew](http://mxcl.github.com/homebrew) installed first, then:

```
brew install emacs --cocoa --srgb --use-git-head --HEAD --with-glib
brew install cask
brew linkapps
git clone git://github.com/waymondo/hemacs ~/.emacs.d 
cd ~/.emacs.d 
cask
open /Applications/Emacs.app
```

