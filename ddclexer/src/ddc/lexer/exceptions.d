module ddc.lexer.exceptions;

import std.conv;

class ParserException : Exception {
    string _msg;
    string _filename;
    int _line;
    int _pos;

    public @property int line() { return _line; }

    this(string msg, string filename, int line, int pos) {
        super(msg ~ " at " ~ filename ~ " line " ~ to!string(line) ~ " column " ~ to!string(pos));
        _msg = msg;
        _filename = filename;
        _line = line;
        _pos = pos;
    }
}

class LexerException : ParserException {
    this(string msg, string filename, int line, int pos) {
        super(msg, filename, line, pos);
    }
}

class SourceEncodingException : LexerException {
    this(string msg, string filename, int line, int pos) {
        super(msg, filename, line, pos);
    }
}
