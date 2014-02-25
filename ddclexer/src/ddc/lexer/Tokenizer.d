module ddc.lexer.Tokenizer;

import ddc.lexer.LineStream;
import ddc.lexer.exceptions;

import std.stdio;

enum TokenType {
	EOF,
	EOL,
	WHITESPACE,
	COMMENT,
	IDENTIFIER,
	STRING,
	CHARACTER,
	INTEGER,
	FLOAT,
	KEYWORD,
	OP
}

class Token {
	protected TokenType _type;
	protected string _file;
	protected uint _line;
	protected uint _pos;
	public @property TokenType type() { return _type; }
	public @property string filename() { return _file; }
	public @property uint line() { return _line; }
	public @property uint pos() { return _pos; }
	public @property wchar[] text() { return null; }
	public @property wchar literalType() { return 0; }
	this(TokenType type) {
		_type = type;
	}
	this(TokenType type, string file, uint line, uint pos) {
		_type = type;
		_file = file;
		_line = line;
		_pos = pos;
	}
	void setPos(string file, uint line, uint pos) {
		_file = file;
		_line = line;
		_pos = pos + 1;
	}
	void setFile(string file) {
		_file = file;
	}
	void setPos(uint line, uint pos) {
		_line = line;
		_pos = pos + 1;
	}
	public abstract Token clone();
}

class EofToken : Token {
	this() {
		super(TokenType.EOF);
	}
	this(string file, uint line, uint pos) {
		super(TokenType.EOF, file, line, pos);
	}
	override public Token clone() {
		return new EofToken(_file, _line, _pos);
	}
}

// treat as white space
//class EolToken : Token {
//	this(string file, uint line, uint pos) {
//		super(TokenType.EOL, file, line, pos);
//	}
//}

class WhiteSpaceToken : Token {
	this() {
		super(TokenType.WHITESPACE);
	}
	this(string file, uint line, uint pos) {
		super(TokenType.WHITESPACE, file, line, pos);
	}
	override public Token clone() {
		return new WhiteSpaceToken(_file, _line, _pos);
	}
}

// do we need comment text?

class CommentToken : Token {
	wchar[] _text;
	public @property override wchar[] text() { return _text; }
	public @property void text(wchar[] text) { _text = text; }
	this() {
		super(TokenType.COMMENT);
	}
	this(string file, uint line, uint pos, wchar[] text) {
		super(TokenType.COMMENT, file, line, pos);
		_text = text;
	}
	override public Token clone() {
		return new CommentToken(_file, _line, _pos, _text);
	}
}

class StringLiteralToken : Token {
	wchar[] _text;
	wchar _literalType;
	public @property override wchar literalType() { return _literalType; }
	public @property override wchar[] text() { return _text; }
	public void setText(wchar[] text, wchar type) { _text = text; _literalType = type; }
	this() {
		super(TokenType.STRING);
	}
	this(string file, uint line, uint pos, wchar[] text, wchar type) {
		super(TokenType.STRING, file, line, pos);
		_text = text;
		_literalType = type;
	}
	override public Token clone() {
		return new StringLiteralToken(_file, _line, _pos, _text.dup, _literalType);
	}
}

// shared appender buffer, to avoid extra heap allocations
struct StringAppender {
	wchar[] buf;
	uint len;
	wchar[] get() {
		return buf[0 .. len];
	}
	void appendEol() {
		if (len + 1 > buf.length) {
			uint newsize = cast(uint)((len + 1 + buf.length) * 2);
			if (newsize < 128)
				newsize = 128;
			buf.length = newsize;
		}
		buf[len] = '\n';
		len++;
	}
	void append(wchar[] s) {
		if (s.length == 0)
			return;
		if (len + s.length > buf.length) {
			uint newsize = cast(uint)((len + s.length + buf.length) * 2);
			if (newsize < 128)
				newsize = 128;
			buf.length = newsize;
		}
		buf[len .. len + s.length] = s;
		len += s.length;
	}
	void reset() {
		len = 0;
	}
}

class Tokenizer
{
	LineStream _lineStream;
	wchar[] _lineText;
	uint _line; // current line number
	uint _len; // current line length
	uint _pos; // current line read position
	uint _state; // tokenizer state
	
	immutable wchar EOF_CHAR = 0x001A;
	immutable wchar EOL_CHAR = 0x000A;
	
	WhiteSpaceToken _sharedWhiteSpaceToken = new WhiteSpaceToken();
	CommentToken _sharedCommentToken = new CommentToken();
	StringLiteralToken _sharedStringLiteralToken = new StringLiteralToken();
	StringAppender _stringLiteralAppender;
	StringAppender _commentAppender;
	
	bool _enableCommentText = true;
	public void enableCommentText(bool enabled) {
		_enableCommentText = enabled;
	}
	
	this(LineStream lineStream) {
		_lineStream = lineStream;
		_sharedWhiteSpaceToken.setFile(_lineStream.filename);
		_sharedCommentToken.setFile(_lineStream.filename);
		_sharedStringLiteralToken.setFile(_lineStream.filename);
	}
	
	// fetch next line from source stream
	bool nextLine() {
		_lineText = _lineStream.readLine();
		if (_lineText is null) {
			if (_lineStream.errorCode != 0)
				throw new SourceEncodingException(_lineStream.errorMessage, _lineStream.filename, _lineStream.errorLine, _lineStream.errorPos);
			_pos = 0;
			_len = 0;
			return false;
		}
		_line = _lineStream.line;
		_pos = 0;
		_len = cast(uint)_lineText.length; // do not support lines longer that 4Gb
		return true;
	}
	
	wchar nextChar() {
		if (_lineText is null) {
			if (!nextLine()) {
				return EOF_CHAR;
			}
		}
		if (_pos >= _len)
			return EOL_CHAR;
		return _lineText[_pos++];
	}
	
	wchar peekChar() {
		if (_lineText is null) {
			if (!nextLine()) {
				return EOF_CHAR;
			}
		}
		if (_pos >= _len)
			return EOL_CHAR;
		return _lineText[_pos++];
	}
	
	Token emitEof() {
		// TODO: check for current state
		return new EofToken(_lineStream.filename, _line, _pos);
	}
	
	Token processWhiteSpace(wchar firstChar) {
		uint line = _line;
		uint pos = _pos - 1;
		for (;;) {
			uint i = _pos;
			for (; i < _len; i++) {
				wchar ch = _lineText[i];
				if (!(ch == 0x0020 || ch == 0x0009 || ch == 0x000B || ch == 0x000C))
					break;
			}
			_pos = i;
			if (_pos < _len)
				break;
			// go to next line
			if (!nextLine())
				break;
		}
		// reuse the same token instance, to avoid extra heap spamming
		_sharedWhiteSpaceToken.setPos(line, pos);
		return _sharedWhiteSpaceToken;
	}
	
	Token processOneLineComment() {
		_sharedCommentToken.setPos(_line, _pos - 1);
		if (_enableCommentText) {
			_sharedCommentToken.text = _lineText[_pos + 1 .. $];
		}
		_pos = _len;
		return _sharedCommentToken;
	}

	// Comment /*   */	
	Token processMultilineComment() {
		_sharedCommentToken.setPos(_line, _pos - 1);
		_commentAppender.reset();
		uint textStart = _pos + 1;
		for (;;) {
			uint textEnd = uint.max;
			uint i = textStart;
			for (; i < _len - 1; i++) {
				if (_lineText[i] == '*' && _lineText[i + 1] == '/') {
					textEnd = i;
					break;
				}
			}
			if (textEnd != uint.max) {
				if (_enableCommentText)
					_commentAppender.append(_lineText[textStart .. textEnd]);
				_pos = textEnd + 2;
				break;
			}
			if (!nextLine()) {
				// TODO: do we need throw exception if comment not closed by end of file?
				_pos = _len;
				break;
			}
			textStart = 0;
		}
		if (_enableCommentText) {
			_sharedCommentToken.text = _commentAppender.get();
		}
		return _sharedCommentToken;
	}
	
	// Comment /*   */	
	Token processNestedComment() {
		_sharedCommentToken.setPos(_line, _pos - 1);
		_commentAppender.reset();
		wchar[] text;
		uint textStart = _pos + 1;
		int level = 1;
		for (;;) {
			uint textEnd = uint.max;
			uint i = textStart;
			for (; i < _len - 1; i++) {
				if (_lineText[i] == '/' && _lineText[i + 1] == '+') {
					level++;
					i++;
				} else if (_lineText[i] == '+' && _lineText[i + 1] == '/') {
					if (--level == 0) {
						textEnd = i;
						break;
					}
				}
			}
			if (textEnd != uint.max) {
				if (_enableCommentText)
					_commentAppender.append(_lineText[textStart .. textEnd]);
				_pos = textEnd + 2;
				break;
			}
			if (!nextLine()) {
				// TODO: do we need throw exception if comment not closed by end of file?
				_pos = _len;
				break;
			}
			if (_enableCommentText)
				_commentAppender.appendEol();
			textStart = 0;
		}
		if (_enableCommentText) {
			_sharedCommentToken.text = _commentAppender.get();
		}
		return _sharedCommentToken;
	}
	
	Token processHexString() {
		_pos++;
		// TODO:
		return null;
	}
	
	Token processDelimitedString() {
		_pos++;
		// TODO:
		return null;
	}
	
	// r"string"   or    `string`
	Token processWysiwygString(wchar ch) {
		_pos++;
		// TODO:
		return null;
	}
	
	void parserError(string msg) {
		throw new ParserException(msg, _lineStream.filename, _line, _pos);
	}
	
	Token processDoubleQuotedString() {
		writeln("processDoubleQuotedString()");
		_sharedStringLiteralToken.setPos(_line, _pos - 1);
		_stringLiteralAppender.reset();
		wchar type = 0;
		for (;;) {
			uint i = _pos;
			uint endPos = uint.max;
			for(; i < _len; i++) {
				if (_lineText[i] == '\"' && (i == 0 || _lineText[i - 1] != '\\')) {
					endPos = i;
					break;
				}
			}
			if (endPos != uint.max) {
				// found end quote
				_stringLiteralAppender.append(_lineText[_pos .. endPos]);
				_pos = endPos + 1;
				break;
			}
			// no quote by end of line
			_stringLiteralAppender.append(_lineText[_pos .. $]);
			_stringLiteralAppender.appendEol();
			if (!nextLine()) {
				// do we need to throw exception if eof comes before end of string?
				break;
			}
		}
		wchar t = 0;
		if (_pos < _len) {
			wchar ch = _lineText[_pos];
			if (ch == 'c' || ch == 'w' || ch == 'd')
				t = ch;
		}
		if (t != 0) {
			if (type != 0 && t != type)
				parserError("Cannot concatenate strings of different type");
			type = t;
		}
		_sharedStringLiteralToken.setText(_stringLiteralAppender.get(), type);
		return _sharedStringLiteralToken;
	}
	
	static bool isUniversalAlpha(wchar ch) {
		if (ch >= 'a' && ch <='z')
			return true;
		if (ch >= 'A' && ch <='Z')
			return true;
		return false;
	}
	
	// returns next token (clone it if you want to store for future usage, otherwise it may be overwritten by further nextToken() calls).
	Token nextToken() {
		wchar ch = nextChar();
		if (ch == EOF_CHAR) {
			return emitEof();
		}
		if (ch == EOL_CHAR || ch == 0x0020 || ch == 0x0009 || ch == 0x000B || ch == 0x000C) {
			// white space (treat EOL as whitespace, too)
			return processWhiteSpace(ch);
		}
		wchar next = _pos < _len ? _lineText[_pos] : 0;
		if (ch == '/') {
			if (next == '/')
				return processOneLineComment();
			else if (next == '*')
				return processMultilineComment();
			else if (next == '+')
				return processNestedComment();
		}
		if (ch == '\"')
			return processDoubleQuotedString();
		if (ch == 'x' && next == '\"')
			return processHexString();
		if (ch == 'q' && next == '\"')
			return processDelimitedString();
		if ((ch == 'r' && next == '\"') || (ch == '`'))
			return processWysiwygString(ch);
		if (ch == '_' || isUniversalAlpha(ch)) {
			// start of identifier or keyword?
		}
		return null;
	}
	
	void setSource(LineStream lineStream) {
		_lineStream = lineStream;
		_line = lineStream.line;
		_pos = 0;
		_lineText = null;
	}
	
}

unittest {
    import std.stdio;
    import std.conv;
    import std.utf;
    string fname = "/home/lve/src/d/ddc/ddclexer/tests/tokenizer_test.d";
	writeln("opening file");
    std.stream.File f = new std.stream.File(fname);
	scope(exit) { f.close(); }
    try {
        LineStream lines = LineStream.create(f, fname);
		Tokenizer tokenizer = new Tokenizer(lines);
	    for (;;) {
			Token token = tokenizer.nextToken();
			if (token is null) {
				writeln("Null token returned");
				break;
			}
			if (token.type == TokenType.EOF) {
				writeln("EOF token");
				break;
			}
			writeln("", token.line, ":", token.pos, "\t", token.type, "\t\"", toUTF8(token.text), "\"");
	    }
    } catch (Exception e) {
        writeln("Exception " ~ e.toString);
    }
}
