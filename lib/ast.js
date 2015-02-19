var Program = function () {

};

var ThreadStatement = function () {

};

var Identifier = function (options) {
    options = options || {};

    this.value = options.value;
};

var AstString = function (options) {
    options = options || {};

    this.value = options.value;
};

var IfStatement = function (options) {

};

var CompoundStatement = function (options) {

};

var StatementList = function (options) {

};

module.exports = {
    Program: Program,
    ThreadStatement: ThreadStatement,
    Identifier: Identifier,
    String: AstString,
    IfStatement: IfStatement,
    CompoundStatement: CompoundStatement,
    StatementList: StatementList,
};
