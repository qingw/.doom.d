#!/bin/bash

echo ">>> making org configurations into *.el"
cp config.org ~/blog/cheng92.com/content/emacs/doom-emacs-with-org.org

# /opt/local/bin/emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "config.org")'
