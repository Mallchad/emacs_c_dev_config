
cemacs  
======
  
cemacs is a configuration for emacs centered around c based language development,  
useful code auto-formatting and highlighing packages are included improves readability   
and reduces repetetive formatting work, as well as easing debugging somewhat.  
  
This project strives to stay close to an standard emacs setup whilst  
adjusting many behaviors to be more user friendly and efficient  
  
# Philosophy
This is my personal config, and its gone through a huge amount of iterations,
currently it's very intentionally geared for me, and only me.

However, despite this, effort  has been put into try to  keep the major features
as  close   to  vanilla,  or  using   as  many  direct  vanilla   mechanisms  as
possible. There  are quite a  few reasons  for this. One  is that it  helps with
familiarity with  a non-configured emacs, which  is helpful to use  from time to
time, especially due to the startup time.

Another is  that a  lot of  vanilla mechanisms  are just  plain better  and more
robust  than most  of the  re-implementations people  write, so  before changing
anything, it's good to consider what is wrong first, and if all the avenues have
been explored.

Another  reason is  a lot  of  emacs functionality  assumes other  parts of  the
ecosystem exists,  for example,  the 'beginning-of-line'  function jumps  to the
absolute beginning  of the  line, rather  than where the  text starts  (which is
usually more useful), it's tempting to replace this function with something that
moves to the  first character on the  line first. But, that  function is already
bound  to 'M-m',  'back-to-indentation'. Of  course, naturally,  `M-m` has  been
rebound     to      multiple     cursors,      and     `C-a`      rebound     to
natural-beginning-of-line. This is something I prefer.

Where changes are made, the focus is on enhancing the vanilla experience, rather
that remaking it.

# Notes on the Differences from Vinilla Emacs    

# Emacs Things You Wish You Knew About
This section just serves to outline some really cool things I thought is worth trying

### Transient Mark Off
Surprisingly emacs ships with the mark being visible, which is actually the
objectively worse way to use emacs. It's more familiar to most selection but it
gets interupted by other commands and vice versa. The non-transient mark is
permanent and sticks around even after running a bunch of (non-mark affecting)
commands. I use `visible-mark-mode` to help visually remind me where I left the mark.

# Credit
It's nice to show some appreciation for  the hacks and help you come across.  As
well as pointing people towards the various places where they might get a little
more information than from here.

This is a hack responsible for the coloured checkbox lines in
org-mode,  
which is largely based on these posts  
- [Fontify done checkbox items in org-mode](https://fuco1.github.io/2017-05-25-Fontify-done-checkbox-items-in-org-mode.html)  
- [Change color of org-mode checkboxes - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/45291/change-color-of-org-mode-checkboxes)  

A simple way of checking if an org heading under the point is folded
[org-folded-p by dan](https://sourcehut.org/consultancy/)
[a](https://sourcehut.org/consultancy/)

'cemacs-desktop-owner-advice'  is a  hack  to disable  checking  for locks  when
reading desktop files. This allows for a bit more flexability in using "desktop"
related functions without using the  overly aggressive 'desktop-mode'  
[Desktop - Emacs Wiki](https://www.emacswiki.org/emacs/Desktop)
