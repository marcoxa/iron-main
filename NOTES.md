# Notes

## 2022-11-20

After much going back and forth, and writing a lot of code that does
not quite work, it appears that the "rigth way" to deal with
continuation lines may be to use an "anchored matcher" for the
continuation mark with a 'pre' code moving the line forward and then
trying to highlight either the *good* or the *bad* continuation line
accordingly.


## 2022-11-26

The anchored matcher seems to work, although there was a problem with
buffer updating and localized editing foen by Font Lock.  The probelm
was that Font Lock started from past the match but before the end of
line and `hlasm-mode--ensure-card-position` was not properly checked.


## 2022-11-29

Anchored matchers seem to work and *change extnesion* takes care of
adjacent lines where the buffer gets modified.

* **Bottom line**: Font Lock gets you there, but it is an enormous
  *gatta da pelare* before you can actually make sense out of it.


## 2023-05-01

Got back to the package and fixed the JCL font-lock colorization.
Still missing: comment syntax tables.


## 2023-06-07

New branch created to change panels.  Still unresolved: strings in JOB
card (and possibly, anywhere in parameters).


## 2023-09-11

Session objects are nice, but I find myself just setfing many
variables as buffer local.  Maybe this is something that should be
cleaned up.


## 2023-09-14

Finally porting the "subpanel" logic to the panels (file
`iron-main-epf.el`).  The logic is rather simple and ad-hoc; trying to
use "atomic windows" and whatnot turned out to be more complex and
unpredictable for my tastes.  If anyone wants to jump in with a
redesign, I'll be happy to adopt it.


## 2023-09-29

Added "column panel logic" working on the "subpanel logic" mentioned
above.  It looks like this could be fully generalized and factored
out.


## 2023-10-05

Scheme works.  It is very sensitive to the `window-height` parameter,
which MUST be specified when creating subpanels.


## 2023-10-23

Decision made to have only the 'Help Panel' using the new scheme with
subpanels.  Other "listing" panels will create new, ad-hoc, panels.
