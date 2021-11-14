
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
usually more useful), it's tempted to  replace this function with something that
moves to the  first character on the  line first. But, that  function is already
bound to 'M-m', 'move-beinning-of-line'.

Where changes are made, the focus is on enhancing the vanilla experience, rather
that remaking it.

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
Alternatively use C-# to "activate" the existing region
  
Keybind Notes  
C-q is now 'hydra-query, a prefix command for searching, C-q s replaces isearch   
with helm-swoop  

# Custom Binding List
Note, this list is massively incomplete, since it is autogenerated from keys bound with
the `bind-key` function, which isn't consistently used everywhere used everywhere in the
config yet.

(Needs to be replaced with an included file)

# Credit
It's nice to show some appreciation for the hacks and help you come  
across.  
As well as pointing people towards the various places where they might  
get a little more information than from here.  

This is a hack responsible for the coloured checkbox lines in
org-mode,  
which is largely based on these posts  
- [Fontify done checkbox items in org-mode](https://fuco1.github.io/2017-05-25-Fontify-done-checkbox-items-in-org-mode.html)  
-[Change color of org-mode checkboxes - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/45291/change-color-of-org-mode-checkboxes)  

A simple way of checking if an org heading under the point is folded
[org-folded-p by dan](https://sourcehut.org/consultancy/)
[a](https://sourcehut.org/consultancy/)

'cemacs-desktop-owner-advice'  is a  hack  to disable  checking  for locks  when
reading desktop files. This allows for a bit more flexability in using "desktop"
related functions without using the  overly aggressive 'desktop-mode'  
[Desktop - Emacs Wiki](https://www.emacswiki.org/emacs/Desktop)
