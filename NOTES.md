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

