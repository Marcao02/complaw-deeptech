#!/usr/bin/env node
"use strict";
var stdio = require('stdio');
var ops = stdio.getopt({
    'form': { key: 'f', description: 'show the full form' },
    'directions': { key: 'd', description: 'show the directions' },
    'content': { key: 'c', description: 'show the content' },
    'schema': { key: 's', description: 'show the schema extracted from directions' },
});
// Make sure we got a filename on the command line.
if (ops.args == undefined || ops.args.length < 1) {
    console.log('Usage: ./read_SAFE.js -s ../generated/SAFE-cap-discount.gen.commonform');
    process.exit(1);
}
// Read the file and print its contents.
var fs = require('fs'), filename = ops.args[0];
var parse = require('commonform-markup-parse');
fs.readFile(filename, 'utf8', function (err, data) {
    if (err)
        throw err;
    var parsed = parse(data); // => {form: Object, directions: Array}
    if (ops.directions) {
        console.log(JSON.stringify(parsed.directions));
    }
    else if (ops.content) {
        console.log(JSON.stringify(parsed.form.content));
    }
    else if (ops.schema) {
        console.log(JSON.stringify(d2s(parsed.directions)));
    }
    else {
        console.log(JSON.stringify(parsed));
    }
});
function d2s(directions) {
    return directions.map(function (d) { return d.identifier; });
}
