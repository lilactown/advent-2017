// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var $$Array          = require("bs-platform/lib/js/array.js");
var Caml_array       = require("bs-platform/lib/js/caml_array.js");
var Caml_format      = require("bs-platform/lib/js/caml_format.js");
var Utils$Advent2017 = require("./Utils.bs.js");

var cases = /* :: */[
  /* tuple */[
    "0\n3\n0\n1\n-3",
    5
  ],
  /* [] */0
];

function jump(stack, _pos, _step) {
  while(true) {
    var step = _step;
    var pos = _pos;
    var exit = 0;
    var offset;
    try {
      offset = Caml_array.caml_array_get(stack, pos);
      exit = 1;
    }
    catch (exn){
      return step;
    }
    if (exit === 1) {
      var pos$prime = pos + offset | 0;
      Caml_array.caml_array_set(stack, pos, offset + 1 | 0);
      _step = step + 1 | 0;
      _pos = pos$prime;
      continue ;
      
    }
    
  };
}

function solve(instructions) {
  var stack = $$Array.map(Caml_format.caml_int_of_string, instructions.split("\n"));
  return jump(stack, 0, 0);
}

var Part1 = /* module */[
  /* cases */cases,
  /* jump */jump,
  /* solve */solve
];

var Part1Test = Utils$Advent2017.Test([
      cases,
      solve
    ]);

var cases$1 = /* :: */[
  /* tuple */[
    "0\n3\n0\n1\n-3",
    10
  ],
  /* [] */0
];

function jump$1(stack, _pos, _step) {
  while(true) {
    var step = _step;
    var pos = _pos;
    var exit = 0;
    var offset;
    try {
      offset = Caml_array.caml_array_get(stack, pos);
      exit = 1;
    }
    catch (exn){
      return step;
    }
    if (exit === 1) {
      if (offset >= 3) {
        var pos$prime = pos + offset | 0;
        Caml_array.caml_array_set(stack, pos, offset - 1 | 0);
        _step = step + 1 | 0;
        _pos = pos$prime;
        continue ;
        
      } else {
        var pos$prime$1 = pos + offset | 0;
        Caml_array.caml_array_set(stack, pos, offset + 1 | 0);
        _step = step + 1 | 0;
        _pos = pos$prime$1;
        continue ;
        
      }
    }
    
  };
}

function solve$1(instructions) {
  var stack = $$Array.map(Caml_format.caml_int_of_string, instructions.split("\n"));
  return jump$1(stack, 0, 0);
}

var Part2 = /* module */[
  /* cases */cases$1,
  /* jump */jump$1,
  /* solve */solve$1
];

var Part2Test = Utils$Advent2017.Test([
      cases$1,
      solve$1
    ]);

var test_part1 = Part1Test[/* check */0];

var test_part2 = Part2Test[/* check */0];

var part1 = solve;

var part2 = solve$1;

exports.Part1      = Part1;
exports.Part1Test  = Part1Test;
exports.Part2      = Part2;
exports.Part2Test  = Part2Test;
exports.part1      = part1;
exports.test_part1 = test_part1;
exports.part2      = part2;
exports.test_part2 = test_part2;
/* Part1Test Not a pure module */