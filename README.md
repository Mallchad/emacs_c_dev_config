
**cemacs**

cemacs is a configuration for emacs centered around c based language development,
useful code auto-formatting and highlighing packages are included improves readability 
and reduces repetetive formatting work, as well as easing debugging somewhat.

This project strives to stay close to an standard emacs setup whilst
adjusting many behaviors to be more user friendly and efficient

# Notes on the Differences from Vinilla Emacs

New Functions
cemacs-kill-volatile-buffer
cemacs-scroll-up-in-place
cemacs-scroll-down-in-place
slay-function
slay-whole-buffer

transient-mark-mode is disabled by default, if you wish to wrap a visible region
press the set mark command twice
C-SPC C-SPC

Keybind Changes
C-j is now 'join-line
M-p is now 'scroll-up-in-place
M-n is now 'scroll-down-in-place
C-s will a now instead pull up 'hydra-slayer, a prefix command that allows you 
to kill entire regions of text more easilly, for text searching see hydra-query
C-q is now 'hydra-query, a prefix command for searching, C-q s replaces isearch 
with helm-swoop
C-r is now 'avy-goto-char this is an extremely efficient way of text navigation
M-r is now 'avy-pop-mark which will navigate through previous locations before
avy enhanced movement
C-x e is now 'crux-find-user-init-file
C-x k now calls a new function 'cemacs-kill-volatile-buffer
C-x a calls 'restart-emacs
