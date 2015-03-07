var fs = require('fs');
var util = require('util');
var parser = require('./morpheus');

var toParse = process.argv[2];
console.log(toParse);
var contents = fs.readFileSync(toParse);
console.log(contents.toString('utf8'));

var ast = parser.parse(contents.toString('utf8'));

debugger;

console.log(util.inspect(ast, false, null));
