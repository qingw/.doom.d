#!/usr/bin/env bash
set -euo pipefail

cd $HOME/.doom.d

emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"README.org\"))"

# $HOME/.gclrc/shl/cp-config-org.sh
