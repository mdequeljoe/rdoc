# 0.1.0.9000

- `rdoc()` now checks for topics passed with the double colon operator (e.g. `rdoc(MASS::bcv)`) (#6).

- Now if `use_rdoc()` has been previously called, re-calling it will move it back to the front of the search path. This addresses the case where another package has also overridden `?` and `help` after `use_rdoc()`.

- `rdoc()` and `rdoc_question()` now take the closest `help` and `?` from the search path respectively rather than always calling them from utils. This allows for cases when another package has defined replacements (e.g. pkgload). 

- `use_rdoc()` now loads the base R library utils to avoid the possibility of being surpassed in the search path at a later point (e.g. on startup if `use_rdoc()` is set in an .Rprofile) (#1).


# 0.1.0

- first release
