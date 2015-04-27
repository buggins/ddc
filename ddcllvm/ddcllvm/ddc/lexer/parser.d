module ddc.lexer.parser;

import ddc.lexer.tokenizer;
import ddc.lexer.ast;


/*

{ }  curlyBlock
( )  parBlock
[ ]  squareBlock
A,A,A
B;B;B;

 */


class Parser {
    Tok[] _tokens;
    int _tokenCount;
    int _pos;

    this(Tok[] tokens) {
        _tokens = tokens;
        _tokenCount = cast(int)_tokens.length;
        _pos = 0;
    }

    bool moreTokens() {
        return _pos < _tokenCount && _tokens[_pos].id != TokId.eof;
    }

    int setBookmark() {
        return _pos;
    }

    void goToBookmark(int bm) {
        _pos = bm;
    }

    void abandonBookmark(int bm) {
    }

    bool currentIs(TokId id) {
        return _tokens[_pos].id == id;
    }

    bool currentIs(TokType t) {
        return _tokens[_pos].type == t;
    }

    bool currentIs(TokType t, uint code) {
        return _tokens[_pos].id == makeTokId(t, code);
    }

    Module parseModule() {
        Module m = new Module();
        return m;
    }

}

Module parseModule(Tok[] tokens) {
    Parser parser = new Parser(tokens);
    Module m = parser.parseModule();
    return m;
}
