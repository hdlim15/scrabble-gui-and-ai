type coordinate = int * int

type letter = char

type cell = {
  coordinate : coordinate;
  letter : letter;
  letter_multiplier : int;
  word_multiplier : int;
}

type board = (cell list) list

type score = int

type bag = letter list

type player = {
  name : string;
  score: int;
  letters : letter list;
}

type state = board * (player list)