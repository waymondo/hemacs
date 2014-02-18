## Hemacs

One man's ever evolving Carbon Emacs 24.3+ setup on OSX. I'm mainly writing Ruby and CoffeeScript, interfacing with Git and Bash, writing compiled stylesheets and templates, and hacking on this elisp dump.

### Installation

You'll need [Homebrew](http://mxcl.github.com/homebrew) and [Cask](http://cask.github.io) installed first, then:

```
brew install emacs --cocoa --use-git-head --HEAD 
brew linkapps 
git clone git://github.com/waymondo/hemacs ~/.emacs.d 
cd ~/.emacs.d 
cask
open /Applications/Emacs.app
```

