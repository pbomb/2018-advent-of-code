// Generated by BUCKLESCRIPT VERSION 5.0.2, PLEASE EDIT WITH CARE
'use strict';

var Util = require("../Util.bs.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");

console.log(Belt_Array.reduce(Belt_Array.map(Util.readLinesOfFile("day1.txt"), (function (prim) {
                return Number(prim);
              })), 0.0, (function (prim, prim$1) {
            return prim + prim$1;
          })));

var frequency = /* () */0;

exports.frequency = frequency;
/*  Not a pure module */
