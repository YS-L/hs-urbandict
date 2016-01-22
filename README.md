[Urban Dictionary](http://www.urbandictionary.com/) in the command line,
implemented in Haskell.

Build
-----
To build and install using [stack](https://github.com/commercialhaskell/stack),
run the following commands in the terminal:

```
stack build
stack test
stack install
```

Usage
-----

* Look up and show the top definition for a word:

  ```
  $ urbandict yolo
  ```

  Output:
  ```
  Looking up the word [yolo]...

  W O R D
  -------

  Yolo

  M E A N I N G
  -------------

  Abbreviation for: you only live once
  The dumbass's excuse for something stupid that they did
  Also one of the most annoying abbreviations ever....

  E X A M P L E
  -------------

  Guy 1: "Hey i heard u got that girl pregnant"

  Dumbass 1: " Ya man but hey YOLO"

  Guy 1: "Hey i heard that you broke ur leg falling off the balcony at that party"
  Dumbass 1: "Ya but hey YOLO"

  A U T H O R
  -----------

  shlubster

  V O T E S
  ---------

  Up: 30394 Down: 8570

  D A T E
  -------

  July 21, 2012

  S O U R C E
  -----------

  http://www.urbandictionary.com/define.php?term=yolo
  ```

* Show today's top definition on the front page:

  ```
  $ urbandict --today
  ```
