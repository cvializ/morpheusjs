var fs = require('fs');
var parser = require('./morpheus');

var contents = fs.readFileSync('./test.scr');
console.log(contents.toString('utf8'));

console.log(parser.parse(contents.toString('utf8')));
