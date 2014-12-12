This repository contains code to build cosponsorship networks from bills passed in the [Bulgarian Parliament](http://www.parliament.bg/).

- [interactive demo](http://briatte.org/bgparl)
- [static plots](http://briatte.org/bgparl/plots.html)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors altogether -- delete both CSV files if you need to refresh the data, which stop in August 2014 with the election of the 43rd legislature. All photos should download fine.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `legislature` -- legislature id (coded in years range instead of actual number)
- `uid` -- bill URL, shortened to numeric id
- `ref` -- another unique identifier
- `date` -- date of introduction of the bill (yyyy-mm-dd)
- `session` -- parliamentary session number (subcomponent of legislatures), in Bulgarian
- `title` -- title, in Bulgarian
- `authors` -- bill sponsors, using their numeric ids separated by semicolons
- `n_au` -- total number of sponsors

Government bills are identified as "GOV" in the `authors` variable. Because no authors are provided for the bills of legislature 39 (2001-2005), the legislature is discarded by the network building routine.

## Sponsors

The sponsors data has one row per legislature in which the sponsor sat, starting with legislature 40 (2005-2009).

- `legisl` -- the legislature number, which matches that of the bills
- `name` -- full name, in English (with some additions from Wikipedia)
- `born` -- year of birth (num)
- `born_bg` -- whether born in Bulgaria (0/1)
- `mandates` -- a string storing the legislatures in which the sponsor sat before `legislature` (the current one)
- `party` -- political party, abbreviated
- `partyname` -- political party, full name
- `job` -- sponsor occupation
- `constituency` -- constituency
- `url` -- URL, shortened to a number stored as text to be used as row names when assigning vertex attributes
- `uid` -- `name` + `url`, used to build the edge list
- `sex` -- gender (F/M), imputed from first and family names ("-ev" and "-ov" for males, "-va" for females)
- `photo` -- photo dummy, always equal to 1 because all photos were successfully downloaded
