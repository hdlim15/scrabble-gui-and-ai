open OUnit2
open State
open Command

let board_1 = [[{cell_coord = (0, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}]]

let board_2 = [[{cell_coord = (0, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
                {cell_coord = (0, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}];
               [{cell_coord = (1, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
                {cell_coord = (1, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}]]

let board_15 = [
  [
    {cell_coord = (0, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (0, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ]; [
    {cell_coord = (1, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (1, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ]; [
    {cell_coord = (2, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (2, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (3, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (3, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (4, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (4, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (5, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (5, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (6, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (6, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (7, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (7, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (8, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (8, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (9, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (9, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (10, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (10, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (11, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (11, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (12, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (12, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (13, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (13, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ];[
    {cell_coord = (14, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 2); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 3); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 4); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 5); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 6); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 7); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 8); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 9); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 10); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 11); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 12); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 13); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
    {cell_coord = (14, 14); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
  ]
]

let player1 = {name = "arman";
               score = 0;
               rack = [('s', 1);('p', 3);('s', 1);('o', 1);('p', 3);('p', 3);('u', 1)];
               player_type = Human}

let player2 = {name = "connor";
               score = 0;
               rack = [('t', 1);('m', 3);('m', 3);('i', 1);('m', 3);('m', 3);('r', 1)];
               player_type = Human}

let basic_state_1bag = {board = board_1;
                        bag = [('z', 10)];
                        players = [player1; player2];
                        added_words = [];
                        current_player = player1}

let basic_state_2bag = {board = board_1;
                        bag = [('z', 10); ('k', 5)];
                        players = [player1; player2];
                        added_words = [];
                        current_player = player1}

let tests = [
  (* init_board tests. *)
  "init_board_1" >:: (fun _ -> assert_equal board_1 (init_board 1));
  "init_board_2" >:: (fun _ -> assert_equal board_2 (init_board 2));
  "init_board_15" >:: (fun _ -> assert_equal board_15 (init_board 15));

  (* do' tests. *)

  (* add_word tests. *)
  "add_word_basic" >:: (fun _ ->
      assert_equal ["blah"] (do' (AddWord "blah") basic_state_1bag).added_words);

  (* swap tests. *)
  "swap1_basic_rack" >:: (fun _ ->
      assert_equal [('z', 10);('p', 3);('s', 1);('o', 1);('p', 3);('p', 3);('u', 1)]
        (do' (Swap ['s']) basic_state_1bag).current_player.rack);
  "swap1_basic_bag" >:: (fun _ ->
      assert_equal [('s', 1)] (do' (Swap ['s']) basic_state_1bag).bag);
  "swap2_basic_rack" >:: (fun _ ->
      let rack' = (do' (Swap ['s'; 'u']) basic_state_2bag).current_player.rack in
      assert_equal true ((List.mem_assoc 'z' rack') && (List.mem_assoc 'k' rack')
                        && List.mem_assoc 's' rack'));
  "swap2_basic_bag" >:: (fun _ ->
      let bag' = (do' (Swap ['s'; 'u']) basic_state_2bag).bag in
      assert_equal true ((List.mem_assoc 's' bag') && (List.mem_assoc 'u' bag')));
  "swap1_exn_bag_too_small" >:: (fun _ ->
      let e = fun () -> do' (Swap ['s';'o']) basic_state_1bag in
      assert_raises InvalidSwap e);
  "swap1_exn_not_in_rack" >:: (fun _ ->
      let e = fun () -> do' (Swap ['l']) basic_state_1bag in
      assert_raises InvalidSwap e);
]
