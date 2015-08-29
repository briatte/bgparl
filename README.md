This repository contains code to build cosponsorship networks from bills passed in the [Bulgarian Parliament](http://www.parliament.bg/).

- [interactive demo](http://f.briatte.org/parlviz/bgparl)
- [static plots](http://f.briatte.org/parlviz/bgparl/plots.html)
- [more countries](https://github.com/briatte/parlnet)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors altogether. All photos should download fine.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `legislature` -- legislature, as years range
- `uid` -- bill URL, shortened to numeric id
- `ref` -- another unique identifier
- `date` -- date of introduction of the bill (yyyy-mm-dd)
- `session` -- parliamentary session number (subcomponent of legislature), in Bulgarian
- `title` -- title, in Bulgarian
- `authors` -- bill sponsors as semicolon-separated numeric ids
- `committee` -- the committee(s) through which the bill has gone

Government bills are identified as "GOV" in the `authors` variable. Because no authors are provided for the bills of legislature 39 (2001-2005), the legislature is discarded by the network building routine.

## Sponsors

The sponsors data has one row per legislature in which the sponsor sat, starting with legislature 40 (2005-2009).

- `legislature` -- legislature number
- `name` -- full name, in English (with some additions from Wikipedia)
- `born` -- year of birth
- `born_bg` -- whether born in Bulgaria (0/1)
- `mandates` -- legislatures in which the sponsor sat before the current one
- `party` -- political party, abbreviated
- `job` -- sponsor occupation
- `constituency` -- constituency, as the string to its Wikipedia English entry
- `url` -- URL, shortened to its numeric uid
- `sex` -- gender (F/M), imputed from first and family names ("-ev" and "-ov" for males, "-va" for females)
- `photo` -- photo, as its local filename
