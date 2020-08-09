# rt-discordian: Discordian Dates

This is a common-lisp system for discordian dates.
It also builds a `ddate` utility like the old linux mainstay.

It definitely works on SBCL and ECL, and at least the library part
almost certainly works on others.

## Building ddate ##

**SBCL**

    (require 'asdf)
	(asdf:make "rt-discordian")

(you might have to continue through some `defconstant`
redefinition warning)

**ECL**

    $ ecl --load build_for_ecl.lisp

## Using ddate ##

Pretty much exactly like the unix utility:

    $ ddate
	Today is Boomtime, the 61st day of Confusion in the YOLD 3186
    
    $ ddate "+%Y-%b"
    3186-Cfn
	
    $ ddate "It's %{%A, the %e of %B%}, %Y. %N%nCelebrate %H" 1995 9 26
    It's Prickle-Prickle, the 50th of Bureaucracy, 3161. 
    Celebrate Bureflux

    $ ddate "Talk about %{%A, the %e of %B%}, %Y. %N%nCelebrate %H" 1996 2 29
    Talk about St. Tib's Day, 3162.
