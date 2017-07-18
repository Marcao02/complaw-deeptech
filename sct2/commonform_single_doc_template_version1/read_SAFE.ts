#!/usr/bin/env node

// Make sure we got a filename on the command line.
if (process.argv.length < 3) {
  console.log('Usage: node read_SAFE.js ../generated/SAFE-cap-discount.gen.commonform');
  process.exit(1);
}
// Read the file and print its contents.
var fs = require('fs'), filename = process.argv[2];

var parse = require('commonform-markup-parse');

fs.readFile(filename, 'utf8', function(err, data) {
  if (err) throw err;
  console.log(data)
  parse(data); // => {form: Object, directions: Array}
});



