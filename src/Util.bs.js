// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Path = require("path");

function readLinesOfFile(filename) {
  var match = typeof __dirname === "undefined" ? undefined : __dirname;
  var dirname = match !== undefined ? match : "";
  return Fs.readFileSync(Path.join(dirname, "inputs", filename), "utf8").split("\n");
}

exports.readLinesOfFile = readLinesOfFile;
/* fs Not a pure module */
