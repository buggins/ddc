module ddc.lexer.tokenizer;

import ddc.lexer.textsource;
import ddc.lexer.exceptions;
import ddc.lexer.unialpha;
import ddc.lexer.linestream;

import std.stdio;
import std.datetime;
import std.conv : to;
import std.utf;
import std.math;
import std.algorithm : equal;

enum TokenType : ubyte {
	EOF,
	//EOL,
	WHITESPACE,
	COMMENT,
	IDENTIFIER,
	STRING,
	CHARACTER,
	INTEGER,
	FLOAT,
	KEYWORD,
	OP,
    INVALID
}

/// character can present at the beginning of identifier
bool isIdentStartChar(dchar ch) pure nothrow {
	return isUniversalAlpha(ch);
}

/// character can present in middle of identifier
bool isIdentMiddleChar(dchar ch) pure nothrow {
	return (ch >= '0' && ch <='9') || isUniversalAlpha(ch);
}

enum CommentCode : uint {
    single,
    multi,
    nested,
}

enum TokType : uint {
    /// end of file
    eof = 0x00000000,
    /// whitespace (code == count of lines whitespace spans, 0 = inside single line)
    whitespace = 0x10000000,
    /// comment
    comment = 0x20000000,
    /// identifier
    ident = 0x30000000,
    /// keyword
    keyword = 0x40000000,
    /// string literal
    str = 0x50000000,
    /// character literal
    character = 0x60000000,
    /// integer literal
    integer = 0x70000000,
    /// floating literal
    floating = 0x80000000,
    /// operator
    op = 0x90000000,
    /// error
    error = 0xF0000000
}

enum CharType : uint {
    unknown = 0,
    char8 = 'c',
    char16 = 'w',
    char32 = 'd'
}

enum TokError : uint {
    none,
    InvalidToken,
    UnexpectedEofInComment,
    InvalidEscapeSequence,
    InvalidStringSuffix,
}

enum TokId : uint {
    eof = TokType.eof,

    whitespace = TokType.whitespace,

    whitespace_singleline = TokType.whitespace,

    ident = TokType.ident,

    comment_single = TokType.comment | CommentCode.single,
    comment_multi = TokType.comment | CommentCode.multi,
    comment_nested = TokType.comment | CommentCode.nested,

    error = TokType.error,
    error_invalidToken = TokType.error | TokError.InvalidToken,
    error_unexpectedEofInComment = TokType.error | TokError.UnexpectedEofInComment,
    error_invalidEscapeSequence = TokType.error | TokError.InvalidEscapeSequence,
    error_invalidStringSuffix = TokType.error | TokError.InvalidStringSuffix,

    str_unknown = TokType.str,
    str_8 = TokType.str | CharType.char8,
    str_16 = TokType.str | CharType.char16,
    str_32 = TokType.str | CharType.char32,

    kw_abstract = TokType.keyword | Keyword.ABSTRACT,
	kw_alias = TokType.keyword | Keyword.ALIAS,
	kw_align = TokType.keyword | Keyword.ALIGN,
	kw_asm = TokType.keyword | Keyword.ASM,
	kw_assert = TokType.keyword | Keyword.ASSERT,
	kw_auto = TokType.keyword | Keyword.AUTO,

	kw_body = TokType.keyword | Keyword.BODY,
	kw_bool = TokType.keyword | Keyword.BOOL,
	kw_break = TokType.keyword | Keyword.BREAK,
	kw_byte = TokType.keyword | Keyword.BYTE,

	kw_case = TokType.keyword | Keyword.CASE,
	kw_cast = TokType.keyword | Keyword.CAST,
	kw_catch = TokType.keyword | Keyword.CATCH,
	kw_cdouble = TokType.keyword | Keyword.CDOUBLE,
	kw_cent = TokType.keyword | Keyword.CENT,
	kw_cfloat = TokType.keyword | Keyword.CFLOAT,
	kw_char = TokType.keyword | Keyword.CHAR,
	kw_class = TokType.keyword | Keyword.CLASS,
	kw_const = TokType.keyword | Keyword.CONST,
	kw_continue = TokType.keyword | Keyword.CONTINUE,
	kw_creal = TokType.keyword | Keyword.CREAL,

	kw_dchar = TokType.keyword | Keyword.DCHAR,
	kw_debug = TokType.keyword | Keyword.DEBUG,
	kw_default = TokType.keyword | Keyword.DEFAULT,
	kw_delegate = TokType.keyword | Keyword.DELEGATE,
	kw_delete = TokType.keyword | Keyword.DELETE,
	kw_deprecated = TokType.keyword | Keyword.DEPRECATED,
	kw_do = TokType.keyword | Keyword.DO,
	kw_double = TokType.keyword | Keyword.DOUBLE,

	kw_else = TokType.keyword | Keyword.ELSE,
	kw_enum = TokType.keyword | Keyword.ENUM,
	kw_export = TokType.keyword | Keyword.EXPORT,
	kw_extern = TokType.keyword | Keyword.EXTERN,

	kw_false = TokType.keyword | Keyword.FALSE,
	kw_final = TokType.keyword | Keyword.FINAL,
	kw_finally = TokType.keyword | Keyword.FINALLY,
	kw_float = TokType.keyword | Keyword.FLOAT,
	kw_for = TokType.keyword | Keyword.FOR,
	kw_foreach = TokType.keyword | Keyword.FOREACH,
	kw_foreach_reverse = TokType.keyword | Keyword.FOREACH_REVERSE,
	kw_function = TokType.keyword | Keyword.FUNCTION,

	kw_goto = TokType.keyword | Keyword.GOTO,

	kw_idouble = TokType.keyword | Keyword.IDOUBLE,
	kw_if = TokType.keyword | Keyword.IF,
	kw_ifloat = TokType.keyword | Keyword.IFLOAT,
	kw_immutable = TokType.keyword | Keyword.IMMUTABLE,
	kw_import = TokType.keyword | Keyword.IMPORT,
	kw_in = TokType.keyword | Keyword.IN,
	kw_inout = TokType.keyword | Keyword.INOUT,
	kw_int = TokType.keyword | Keyword.INT,
	kw_interface = TokType.keyword | Keyword.INTERFACE,
	kw_invariant = TokType.keyword | Keyword.INVARIANT,
	kw_ireal = TokType.keyword | Keyword.IREAL,
	kw_is = TokType.keyword | Keyword.IS,

	kw_lazy = TokType.keyword | Keyword.LAZY,
	kw_long = TokType.keyword | Keyword.LONG,

	kw_macro = TokType.keyword | Keyword.MACRO,
	kw_mixin = TokType.keyword | Keyword.MIXIN,
	kw_module = TokType.keyword | Keyword.MODULE,

	kw_new = TokType.keyword | Keyword.NEW,
	kw_nothrow = TokType.keyword | Keyword.NOTHROW,
	kw_null = TokType.keyword | Keyword.NULL,

	kw_out = TokType.keyword | Keyword.OUT,
	kw_override = TokType.keyword | Keyword.OVERRIDE,

	kw_package = TokType.keyword | Keyword.PACKAGE,
	kw_pragma = TokType.keyword | Keyword.PRAGMA,
	kw_private = TokType.keyword | Keyword.PRIVATE,
	kw_protected = TokType.keyword | Keyword.PROTECTED,
	kw_public = TokType.keyword | Keyword.PUBLIC,
	kw_pure = TokType.keyword | Keyword.PURE,

	kw_real = TokType.keyword | Keyword.REAL,
	kw_ref = TokType.keyword | Keyword.REF,
	kw_return = TokType.keyword | Keyword.RETURN,

	kw_scope = TokType.keyword | Keyword.SCOPE,
	kw_shared = TokType.keyword | Keyword.SHARED,
	kw_short = TokType.keyword | Keyword.SHORT,
	kw_static = TokType.keyword | Keyword.STATIC,
	kw_struct = TokType.keyword | Keyword.STRUCT,
	kw_super = TokType.keyword | Keyword.SUPER,
	kw_switch = TokType.keyword | Keyword.SWITCH,
	kw_synchronized = TokType.keyword | Keyword.SYNCHRONIZED,

	kw_template = TokType.keyword | Keyword.TEMPLATE,
	kw_this = TokType.keyword | Keyword.THIS,
	kw_throw = TokType.keyword | Keyword.THROW,
	kw_true = TokType.keyword | Keyword.TRUE,
	kw_try = TokType.keyword | Keyword.TRY,
	kw_typedef = TokType.keyword | Keyword.TYPEDEF,
	kw_typeid = TokType.keyword | Keyword.TYPEID,
	kw_typeof = TokType.keyword | Keyword.TYPEOF,

	kw_ubyte = TokType.keyword | Keyword.UBYTE,
	kw_ucent = TokType.keyword | Keyword.UCENT,
	kw_uint = TokType.keyword | Keyword.UINT,
	kw_ulong = TokType.keyword | Keyword.ULONG,
	kw_union = TokType.keyword | Keyword.UNION,
	kw_unittest = TokType.keyword | Keyword.UNITTEST,
	kw_ushort = TokType.keyword | Keyword.USHORT,

	kw_version = TokType.keyword | Keyword.VERSION,
	kw_void = TokType.keyword | Keyword.VOID,
	kw_volatile = TokType.keyword | Keyword.VOLATILE,

	kw_wchar = TokType.keyword | Keyword.WCHAR,
	kw_while = TokType.keyword | Keyword.WHILE,
	kw_with = TokType.keyword | Keyword.WITH,

	kw_file = TokType.keyword | Keyword.FILE,
	kw_module__ = TokType.keyword | Keyword.MODULE__,
	kw_line = TokType.keyword | Keyword.LINE,
	kw_function__ = TokType.keyword | Keyword.FUNCTION__,
	kw_pretty_function = TokType.keyword | Keyword.PRETTY_FUNCTION,

	//Special Token	Replaced with
	kw_date = TokType.keyword | Keyword.DATE, //	string literal of the date of compilation "mmm dd yyyy"
	kw_eof = TokType.keyword | Keyword.EOF, //	sets the scanner to the end of the file
	kw_time = TokType.keyword | Keyword.TIME, //	string literal of the time of compilation "hh:mm:ss"
	kw_timestamp = TokType.keyword | Keyword.TIMESTAMP, //	string literal of the date and time of compilation "www mmm dd hh:mm:ss yyyy"
	kw_vendor = TokType.keyword | Keyword.VENDOR, //	Compiler vendor string, such as "Digital Mars D"
	kw_version__ = TokType.keyword | Keyword.VERSION_, //	Compiler version as an integer, such as 2001
	
	kw_gshared = TokType.keyword | Keyword.GSHARED,
	kw_traits = TokType.keyword | Keyword.TRAITS,
	kw_vector = TokType.keyword | Keyword.VECTOR,
	kw_parameters = TokType.keyword | Keyword.PARAMETERS,


    // operator IDs
  	op_div = TokType.op | OpCode.DIV, 		            //    /
	op_div_eq = TokType.op | OpCode.DIV_EQ, 	        //    /=
	op_dot = TokType.op | OpCode.DOT, 		            //    .
	op_dot_dot = TokType.op | OpCode.DOT_DOT, 	        //    ..
	op_dot_dot_dot = TokType.op | OpCode.DOT_DOT_DOT,   //    ...
	op_and = TokType.op | OpCode.AND, 		            //    &
	op_and_eq = TokType.op | OpCode.AND_EQ, 	        //    &=
	op_log_and = TokType.op | OpCode.LOG_AND, 	        //    &&
	op_or = TokType.op | OpCode.OR, 		            //    |
	op_or_eq = TokType.op | OpCode.OR_EQ, 		        //    |=
	op_log_or = TokType.op | OpCode.LOG_OR, 	        //    ||
	op_minus = TokType.op | OpCode.MINUS, 		        //    -
	op_minus_eq = TokType.op | OpCode.MINUS_EQ, 	    //    -=
	op_minus_minus = TokType.op | OpCode.MINUS_MINUS,   //    --
	op_plus = TokType.op | OpCode.PLUS, 		        //    +
	op_plus_eq = TokType.op | OpCode.PLUS_EQ, 	        //    +=
	op_plus_plus = TokType.op | OpCode.PLUS_PLUS, 	    //    ++
	op_lt = TokType.op | OpCode.LT, 		            //    <
	op_lt_eq = TokType.op | OpCode.LT_EQ, 		        //    <=
	op_shl = TokType.op | OpCode.SHL, 		            //    <<
	op_shl_eq = TokType.op | OpCode.SHL_EQ, 	        //    <<=
	op_lt_gt = TokType.op | OpCode.LT_GT, 		        //    <>
	op_ne_eq = TokType.op | OpCode.NE_EQ, 		        //    <>=
	op_gt = TokType.op | OpCode.GT, 		            //    >
	op_gt_eq = TokType.op | OpCode.GT_EQ, 		        //    >=
	op_shr_eq = TokType.op | OpCode.SHR_EQ,		        //    >>=
	op_asr_eq = TokType.op | OpCode.ASR_EQ, 	        //    >>>=
	op_shr = TokType.op | OpCode.SHR, 		            //    >>
	op_asr = TokType.op | OpCode.ASR, 		            //    >>>
	op_not = TokType.op | OpCode.NOT, 		            //    !
	op_not_eq = TokType.op | OpCode.NOT_EQ,		        //    !=
	op_not_lt_gt = TokType.op | OpCode.NOT_LT_GT, 	    //    !<>
	op_not_lt_gt_eq = TokType.op | OpCode.NOT_LT_GT_EQ, //    !<>=
	op_not_lt = TokType.op | OpCode.NOT_LT, 	        //    !<
	op_not_lt_eq = TokType.op | OpCode.NOT_LT_EQ, 	    //    !<=
	op_not_gt = TokType.op | OpCode.NOT_GT, 	        //    !>
	op_not_gt_eq = TokType.op | OpCode.NOT_GT_EQ, 	    //    !>=
	op_par_open = TokType.op | OpCode.PAR_OPEN, 	    //    (
	op_par_close = TokType.op | OpCode.PAR_CLOSE, 	    //    )
	op_sq_open = TokType.op | OpCode.SQ_OPEN, 	        //    [
	op_sq_close = TokType.op | OpCode.SQ_CLOSE, 	    //    ]
	op_curl_open = TokType.op | OpCode.CURL_OPEN, 	    //    {
	op_curl_close = TokType.op | OpCode.CURL_CLOSE,     //    }
	op_quest = TokType.op | OpCode.QUEST, 		        //    ?
	op_comma = TokType.op | OpCode.COMMA, 		        //    ,
	op_semicolon = TokType.op | OpCode.SEMICOLON,       //    ;
	op_colon = TokType.op | OpCode.COLON, 	            //    :
	op_dollar = TokType.op | OpCode.DOLLAR, 	        //    $
	op_eq = TokType.op | OpCode.EQ, 		            //    =
	op_eq_eq = TokType.op | OpCode.QE_EQ, 		        //    ==
	op_mul = TokType.op | OpCode.MUL, 		            //    *
	op_mul_eq = TokType.op | OpCode.MUL_EQ, 	        //    *=
	op_mod = TokType.op | OpCode.MOD, 	                //    %
	op_mod_eq = TokType.op | OpCode.MOD_EQ,             //    %=
	op_xor = TokType.op | OpCode.XOR, 		            //    ^
	op_xor_eq = TokType.op | OpCode.XOR_EQ, 	        //    ^=
	op_log_xor = TokType.op | OpCode.LOG_XOR, 	        //    ^^
	op_log_xor_eq = TokType.op | OpCode.LOG_XOR_EQ,     //    ^^=
	op_inv = TokType.op | OpCode.INV, 		            //    ~
	op_inv_eq = TokType.op | OpCode.INV_EQ, 	        //    ~=
	op_at = TokType.op | OpCode.AT, 		            //    @
	op_eq_gt = TokType.op | OpCode.EQ_GT, 		        //    =>
	op_sharp = TokType.op | OpCode.SHARP, 		        //    #
}

struct StringCache {
    private string[] _idToName;
    private uint[string] _nameToId;
    private uint _nextId = 1;
    uint intern(string s) {
        if (!s.length)
            return 0;
        if (auto p = s in _nameToId) {
            return *p;
        }
        _nameToId[s] = _nextId;
        if (_idToName.length <= _nextId)
            _idToName.length = _idToName.length == 0 ? 256 : _idToName.length * 2;
        _idToName[_nextId] = s.dup;
        return _nextId++;
    }
    string get(uint id) {
        if (id == 0 || id >= _nextId)
            return null;
        return _idToName[id];
    }
}

// 20 - 32 bytes
/// token structure
struct Tok {
    // 4 bytes
    /// holds both type[4bit] + code[28 bit]
    uint id; 
    @property TokType type() const { return cast(TokType)(id & 0xF0000000); }
    @property uint code() const { return (id & 0x0FFFFFFF); }
    @property void type(TokType t) {
        id = ((cast(uint)t) & 0xF0000000) | (id & 0x0FFFFFFF); 
    }
    void setType(TokType t, uint code) {
        id = ((cast(uint)t) & 0xF0000000) | (code & 0x0FFFFFFF); 
    }
    // 4 bytes
    /// 0-based byte offset from beginning of utf-8 encoded line
    int pos;
    // 4-8 bytes
    /// reference to source line and file
    const(SourceLine) * line;
    // 8 - 16 bytes
    // value is stored as string, convert on demand
    string str;

    /// returns type or full id for tokenizer testing, depending on type
    @property uint idForTest() {
        switch(type) {
            case TokType.op:
            case TokType.comment:
            case TokType.keyword:
            case TokType.str:
                return id;
            default:
                return type();
        }
    }

    string toString() const {
        string postext = to!string(line.line + 1) ~ ":" ~ to!string(pos + 1) ~  " ";
        switch(type) {
            case TokType.op:
            case TokType.comment:
            case TokType.keyword:
            case TokType.str:
                return postext ~ to!string(cast(TokId)id) ~ " " ~ str;
            default:
                return postext ~ to!string(type()) ~ " " ~ str;
        }
    }
}

enum StringTokenMode {
    /// include quotes and character types, no scape sequence processing - e.g. r"bla\nbla"w
    raw,
    /// exclude quotes and character types, no scape sequence processing - e.g. bla\nbla
    rawnoquotes,
    /// exclude quotes and character types, perform escape sequence processing - e.g. bla<newline>bla
    processed
}

struct Utf8Tokenizer {
    private StringCache * _identCache;
    private TextLines _source;
    private const(SourceLine) * _line;
    private int _lineIndex;
    private string _lineText;
    private int _lineLen;
    private int _pos;
    private Tok _token;
    private StringTokenMode _stringTokenMode = StringTokenMode.processed;

    /// string literal content processing mode
    @property StringTokenMode stringTokenMode() { return _stringTokenMode; }
    /// string literal content processing mode
    @property void stringTokenMode(StringTokenMode mode) { _stringTokenMode = mode; }


    void init(StringCache * identCache, TextLines source) {
        _identCache = identCache;
        _source = source;
        _lineIndex = -1;
        nextLine();
    }
    
    void startToken() {
        _token.id = TokId.eof;
        _token.line = _line;
        _token.pos = _pos;
        _token.str = null;
    }

    void updateTokenText() {
        _token.str = _source.rangeText(_token.line.line, _token.pos, _lineIndex, _pos);
    }

    bool nextLine() {
        if (_lineIndex + 1 >= _source.lineCount)
            return false;
        _lineIndex++;
        _line = _source.line(_lineIndex);        
        _lineText = _line.text;
        _lineLen = cast(int)_lineText.length;
        _pos = 0;
        return true;
    }

    void parseWhitespace() {
        int lineCount = 0;
        for (;;) {
            while (_pos >= _lineLen) {
                // skip lines
                if (!nextLine()) {
                    if (_lineIndex > _token.line.line || _pos > _token.pos) {
                        // return whitespace; EOF will be returned in next call
                        _token.setType(TokType.whitespace, _lineIndex - _token.line.line);
                        updateTokenText();
                    }
                    // returning whitespace or eof
                    return;
                }
            }
            assert(_pos < _lineLen);
            char ch = _lineText[_pos];
		    if (ch == 0x0020 || ch == 0x0009 || ch == 0x000B || ch == 0x000C) {
			    // white space (treat EOL as whitespace, too)
                _pos++;
		    } else {
                break;
            }
        }
        _token.setType(TokType.whitespace, _lineIndex - _token.line.line);
        updateTokenText();
    }

    void parseSingleLineComment() {
        _token.id = TokId.comment_single;
        _pos = _lineLen;
        updateTokenText();
    }

    void parseMultilineComment() {
        _pos += 2;
        for (;;) {
            while (_pos >= _lineLen) {
                if (!nextLine()) {
                    _token.id = TokId.error_unexpectedEofInComment;
                    updateTokenText();
                    return;
                }
            }
            char ch = _lineText[_pos];
            char ch2 = _pos + 1 < _lineLen ? _lineText[_pos + 1] : 0;
            if (ch == '*' && ch2 == '/') {
                _token.id = TokId.comment_multi;
                _pos += 2;
                updateTokenText();
                return;
            }
            _pos++;
        }
    }

    void parseNestedComment() {
        _pos += 2;
        int nesting = 1;
        for (;;) {
            while (_pos >= _lineLen) {
                if (!nextLine()) {
                    _token.id = TokId.error_unexpectedEofInComment;
                    updateTokenText();
                    return;
                }
            }
            char ch = _lineText[_pos];
            char ch2 = _pos + 1 < _lineLen ? _lineText[_pos + 1] : 0;
            if (ch == '/' && ch2 == '+') {
                nesting++;
                _pos++;
            } else if (ch == '+' && ch2 == '/') {
                nesting--;
                if (nesting == 0) {
                    _token.id = TokId.comment_nested;
                    _pos += 2;
                    updateTokenText();
                    return;
                }
                _pos++;
            }
            _pos++;
        }
    }

    void errorToken() {
        _pos++;
        _token.id = TokId.error_invalidToken;
        updateTokenText();
    }

    /// decode UTF8 to UTF32 character from current position, provide next position in line
    dchar decodeChar(ref int nextPos) {
        uint ch0 = _lineText[_pos];
        if ((ch0 & 0x80) == 0) {
            nextPos = _pos + 1;
            return ch0;
        }
        uint ch1 = _lineText[_pos + 1];
        if ((ch0 & 0xE0) == 0xC0) {
            nextPos = _pos + 2;
            return ((ch0 & 0x1F) << 6) | ((ch1 & 0x3F));
        } 
        uint ch2 = _lineText[_pos + 2];
        if ((ch0 & 0xE0) == 0xE0) {
            nextPos = _pos + 3;
            return ((ch0 & 0x0F) << 12) | ((ch1 & 0x1F) << 6) | ((ch2 & 0x3F));
        }
        uint ch3 = _lineText[_pos + 3];
        nextPos = _pos + 4;
        return ((ch0 & 0x07) << 18) | ((ch1 & 0x3F) << 12) | ((ch2 & 0x3F) << 6) | ((ch3 & 0x3F));
    }

	protected OpCode detectOp() nothrow {
		char ch = _lineText[_pos];
		if (ch >= 128)
			return OpCode.NONE;
		char ch2 = _pos + 1 < _lineLen ? _lineText[_pos + 1] : 0;
		char ch3 = _pos + 2 < _lineLen ? _lineText[_pos + 2] : 0;
		switch(cast(ubyte)ch) {
			//	DIV, 		//    /
			//	DIV_EQ, 	//    /=
			case '/':
				if (ch2 == '=') {
					_pos += 2;
					return OpCode.DIV_EQ;
				}
				return OpCode.DIV;
			//	DOT, 		//    .
			//	DOT_DOT, 	//    ..
			//	DOT_DOT_DOT,//    ...
			case '.':
				if (ch2 == '.') {
					if (ch3 == '.') {
						_pos += 3;
						return OpCode.DOT_DOT_DOT;
					}
			        _pos += 2;
					return OpCode.DOT_DOT;
				}
    			_pos += 1;
				return OpCode.DOT;
			//	AND, 		//    &
			//	AND_EQ, 	//    &=
			//	LOG_AND, 	//    &&
			case '&':
				if (ch2 == '=') {
    				_pos += 2;
					return OpCode.AND_EQ;
				}
				if (ch2 == '&') {
    				_pos += 2;
					return OpCode.LOG_AND;
				}
  				_pos += 1;
				return OpCode.AND;
			//	OR, 		//    |
			//	OR_EQ, 		//    |=
			//	LOG_OR, 	//    ||
			case '|':
				if (ch2 == '=') {
    				_pos += 2;
					return OpCode.OR_EQ;
				}
				if (ch2 == '|') {
    				_pos += 2;
					return OpCode.LOG_OR;
				}
  				_pos += 1;
				return OpCode.OR;
			//	MINUS, 		//    -
			//	MINUS_EQ, 	//    -=
			//	MINUS_MINUS,//    --
			case '-':
				if (ch2 == '=') {
    				_pos += 2;
					return OpCode.MINUS_EQ;
				}
				if (ch2 == '-') {
    				_pos += 2;
					return OpCode.MINUS_MINUS;
				}
				return OpCode.MINUS;
			//	PLUS, 		//    +
			//	PLUS_EQ, 	//    +=
			//	PLUS_PLUS, 	//    ++
			case '+':
				if (ch2 == '=') {
    				_pos += 2;
					return OpCode.PLUS_EQ;
				}
				if (ch2 == '+') {
    				_pos += 2;
					return OpCode.PLUS_PLUS;
				}
  				_pos += 1;
				return OpCode.PLUS;
			//	LT, 		//    <
			//	LT_EQ, 		//    <=
			//	SHL, 		//    <<
			//	SHL_EQ, 	//    <<=
			//	LT_GT, 		//    <>
			//	NE_EQ, 		//    <>=
			case '<':
				if (ch2 == '<') {
					if (ch3 == '=') {
        				_pos += 3;
						return OpCode.SHL_EQ;
					}
    				_pos += 2;
					return OpCode.SHL;
				}
				if (ch2 == '>') {
					if (ch3 == '=') {
        				_pos += 3;
						return OpCode.NE_EQ;
					}
    				_pos += 2;
					return OpCode.LT_GT;
				}
				if (ch2 == '=') {
    				_pos += 2;
					return OpCode.LT_EQ;
				}
   				_pos += 1;
				return OpCode.LT;
			//	GT, 		//    >
			//	GT_EQ, 		//    >=
			//	SHR_EQ		//    >>=
			//	ASR_EQ, 	//    >>>=
			//	SHR, 		//    >>
			//	ASR, 		//    >>>
			case '>':
				if (ch2 == '>') {
					if (ch3 == '>') {
						dchar ch4 = _pos + 3 < _lineLen ? _lineText[_pos + 3] : 0;
						if (ch4 == '=') { // >>>=
            				_pos += 4;
							return OpCode.ASR_EQ;
						}
        				_pos += 3;
						return OpCode.ASR; // >>>
					}
					if (ch3 == '=') { // >>=
        				_pos += 3;
						return OpCode.SHR_EQ;
					}
    				_pos += 2;
					return OpCode.SHR;
				}
				if (ch2 == '=') { // >=
    				_pos += 2;
					return OpCode.GT_EQ;
				}
				// >
   				_pos += 1;
				return OpCode.GT;
			//	NOT, 		//    !
			//	NOT_EQ		//    !=
			//	NOT_LT_GT, 	//    !<>
			//	NOT_LT_GT_EQ, //    !<>=
			//	NOT_LT, 	//    !<
			//	NOT_LT_EQ, 	//    !<=
			//	NOT_GT, 	//    !>
			//	NOT_GT_EQ, 	//    !>=
			case '!':
				if (ch2 == '<') { // !<
					if (ch3 == '>') { // !<>
						dchar ch4 = _pos + 3 < _lineLen ? _lineText[_pos + 3] : 0;
						if (ch4 == '=') { // !<>=
            				_pos += 4;
							return OpCode.NOT_LT_GT_EQ;
						}
        				_pos += 3;
						return OpCode.NOT_LT_GT; // !<>
					}
					if (ch3 == '=') { // !<=
        				_pos += 3;
						return OpCode.NOT_LT_EQ;
					}
    				_pos += 2;
					return OpCode.NOT_LT; // !<
				}
				if (ch2 == '=') { // !=
    				_pos += 2;
					return OpCode.NOT_EQ;
				}
   				_pos += 1;
				return OpCode.NOT;
			//	PAR_OPEN, 	//    (
			case '(':
   				_pos += 1;
				return OpCode.PAR_OPEN;
			//	PAR_CLOSE, 	//    )
			case ')':
   				_pos += 1;
				return OpCode.PAR_CLOSE;
			//	SQ_OPEN, 	//    [
			case '[':
   				_pos += 1;
				return OpCode.SQ_OPEN;
			//	SQ_CLOSE, 	//    ]
			case ']':
   				_pos += 1;
				return OpCode.SQ_CLOSE;
			//	CURL_OPEN, 	//    {
			case '{':
   				_pos += 1;
				return OpCode.CURL_OPEN;
			//	CURL_CLOSE, //    }
			case '}':
   				_pos += 1;
				return OpCode.CURL_CLOSE;
			//	QUEST, 		//    ?
			case '?':
   				_pos += 1;
				return OpCode.QUEST;
			//	COMMA, 		//    ,
			case ',':
   				_pos += 1;
				return OpCode.COMMA;
			//	SEMICOLON, 	//    ;
			case ';':
   				_pos += 1;
				return OpCode.SEMICOLON;
			//	COLON, 	    //    :
			case ':':
   				_pos += 1;
				return OpCode.COLON;
			//	DOLLAR, 	//    $
			case '$':
   				_pos += 1;
				return OpCode.DOLLAR;
			//	EQ, 		//    =
			//	QE_EQ, 		//    ==
			//	EQ_GT, 		//    =>
			case '=':
				if (ch2 == '=') { // ==
       				_pos += 2;
					return OpCode.QE_EQ;
				}
				if (ch2 == '>') { // =>
       				_pos += 2;
					return OpCode.EQ_GT;
				}
   				_pos += 1;
				return OpCode.EQ;
			//	MUL, 		//    *
			//	MUL_EQ, 	//    *=
			case '*':
				if (ch2 == '=') {
       				_pos += 2;
					return OpCode.MUL_EQ;
				}
   				_pos += 1;
				return OpCode.MUL;
			//	MOD, 	//    %
			//	MOD_EQ, //    %=
			case '%':
				if (ch2 == '=') {
       				_pos += 2;
					return OpCode.MOD_EQ;
				}
   				_pos += 1;
				return OpCode.MOD;
			//	XOR, 		//    ^
			//	XOR_EQ, 	//    ^=
			//	LOG_XOR, 	//    ^^
			//	LOG_XOR_EQ, //    ^^=
			case '^':
				if (ch2 == '^') {
					if (ch3 == '=') {
						_pos += 3;
						return OpCode.LOG_XOR_EQ;
					}
       				_pos += 2;
					return OpCode.LOG_XOR;
				}
				if (ch2 == '=') {
       				_pos += 2;
					return OpCode.XOR_EQ;
				}
   				_pos += 1;
				return OpCode.XOR;
			//	INV, 		//    ~
			//	INV_EQ, 	//    ~=
			case '~':
				if (ch2 == '=') {
       				_pos += 2;
					return OpCode.INV_EQ;
				}
   				_pos += 1;
				return OpCode.INV;
			//	AT, 		//    @
			case '@':
   				_pos += 1;
				return OpCode.AT;
			//	SHARP 		//    #
			case '#':
   				_pos += 1;
				return OpCode.SHARP;
			default:
				return OpCode.NONE;
		}
	}
	
    void parseIdentOrKeyword() {
        // first character is already skipped
        int nextPos;
        while (_pos < _lineLen) {
            dchar dch = decodeChar(nextPos);
            if (!isIdentMiddleChar(dch))
                break;
            _pos = nextPos;
        }
        _token.id = TokType.ident;
        updateTokenText();
        Keyword kw = findKeyword(_token.str);
        if (kw != Keyword.NONE)
            _token.setType(TokType.keyword, kw);
        else {
            uint id = _identCache.intern(_token.str);
            _token.str = _identCache.get(id);
            _token.setType(TokType.ident, id);
        }
    }

    /// remove quotes and char type from string literal, e.g.   r"bla bla"w --> bla bla
    private static string removeQuotes(string s) {
        int skipStart = 0;
        int skipEnd = 0;
        char firstchar = s[0];
        char lastchar = s[$ - 1];
        if (lastchar == 'c' || lastchar == 'd' || lastchar == 'w') {
            skipEnd++;
            lastchar = s[$ - 1 - skipEnd];
        }
        if (firstchar == 'r') {
            skipStart++;
            firstchar = s[skipStart];
        }
        if ((firstchar == '\"' && lastchar == '\"') || (firstchar == '`' && lastchar == '`')) {
            skipStart++;
            skipEnd++;
        }
        return s[skipStart .. $ - skipEnd];
    }

    static int parseHexDigit(dchar ch) {
        if (ch >= '0' && ch <='9')
            return ch - '0';
        if (ch >= 'a' && ch <='f')
            return ch - 'a' + 10;
        if (ch >= 'A' && ch <='F')
            return ch - 'A' + 10;
        return -1;
    }
    static dchar decodeHex(string s, ref int pos, int count, ref bool errorFlag) {
        dchar res = 0;
        for (int i = 0; i < count; i++) {
            if (pos + 1 >= s.length) {
                errorFlag = true;
                return res;
            }
            dchar ch = s[++pos];
            int digit = parseHexDigit(ch);
            if (digit < 0) {
                errorFlag = true;
                digit = 0;
            }
            res = (res << 4) | digit;
        }
        if (res >= 0x100000) // too big for unicode character
            errorFlag = true;
        return res;
    }

    static dchar decodeOct(string s, dchar firstChar, ref int pos, ref bool errorFlag) {
        dchar res = 0;
        res = firstChar - '0';
        if (pos + 1 < s.length && s[pos + 1] >= '0' && s[pos + 1] <= '7') {
            res = (res << 3) | (s[++pos] - '0');
        }
        if (pos + 1 < s.length && s[pos + 1] >= '0' && s[pos + 1] <= '7') {
            res = (res << 3) | (s[++pos] - '0');
        }
        return res;
    }

    char[] entityNameBuf;
    int entityNameLen;

    static dchar decodeCharacterEntity(string s, ref int pos, ref bool errorFlag) {
        pos++;
        int start = pos;
        for(; pos < s.length && s[pos] != ';'; pos++) {
            char ch = s[pos];
            if (ch >= 0x80)
                errorFlag = true;
        }
        if (pos < s.length && s[pos] == ';') {
            dchar ch = entityToChar(s[start .. pos]);
            if (ch)
                return ch;
        }
        errorFlag = true;
        return '?';
    }

    static bool processEscapeSequences(ref string s) {
        bool backslashFound = false;
        foreach(ch; s)
            if (ch == '\\') {
                backslashFound = true;
                break;
            }
        if (!backslashFound)
            return true;
        bool errorFlag = false;
        int len = cast(int)s.length;
        char[] buf;
        buf.reserve(s.length);
        int dst = 0;
        for (int src = 0; src < len; src++) {
            dchar ch = s[src];
            if (ch == '\\') {
                if (src == len - 1)
                    break; // INVALID
                ch = s[++src];
                switch (ch) {
                    case '\'':
                    case '\"':
                    case '?':
                    case '\\':
                        // leave ch as is
                        break;
                    case '0':
                        ch = '\0';
                        break;
                    case 'a':
                        ch = '\a';
                        break;
                    case 'b':
                        ch = '\b';
                        break;
                    case 'f':
                        ch = '\f';
                        break;
                    case 'n':
                        ch = '\n';
                        break;
                    case 'r':
                        ch = '\r';
                        break;
                    case 't':
                        ch = '\t';
                        break;
                    case 'v':
                        ch = '\v';
                        break;
                    case 'x':
                        ch = decodeHex(s, src, 2, errorFlag);
                        break;
                    case 'u':
                        ch = decodeHex(s, src, 4, errorFlag);
                        break;
                    case 'U':
                        ch = decodeHex(s, src, 8, errorFlag);
                        break;
                    default:
                        if (ch >= '0' && ch <= '7') {
                            // octal X XX or XXX
                            ch = decodeOct(s, ch, src, errorFlag); // something wrong
                        } else if (ch == '&') {
                            // named character entity
                            ch = decodeCharacterEntity(s, src, errorFlag);
                            // just show it as is
                        } else {
                            errorFlag = true;
                        }
                        break;
                }
            }
            // encode ch to utf8 and put into buffer
            if (ch < 0x80) {
                buf ~= cast(char)ch;
            } else {
                // encode utf8
                char[4] enc;
                auto sz = encode(enc, ch);
                for(int i = 0; i < sz; i++)
                    buf ~= enc[i];
            }
        }
        if (errorFlag) {
            return false;
        }
        // no error: copy result string
        s = buf.dup;
        return !errorFlag;
    }

    void parseStringLiteral() {
        char delimiter = _lineText[_pos];
		bool wysiwyg = (delimiter == 'r' || delimiter == '`');
		_pos++;
		if (delimiter == 'r') {
			_pos++;
			delimiter = '\"';
		}
		dchar type = 0;
		for (;;) {
			int i = _pos;
			int endPos = int.max;
            bool lastBackSlash = false;
			for (; i < _lineLen; i++) {
                char ch = _lineText[i];
                if (ch == '\\') {
                    if (lastBackSlash)
                        lastBackSlash = false;
                    else
                        lastBackSlash = true;
                } else if (ch == delimiter && !lastBackSlash) {
					endPos = i;
					break;
				}
                else if (lastBackSlash)
                    lastBackSlash = false;
			}
			if (endPos != int.max) {
				// found end quote
				_pos = endPos + 1;
				break;
			}
			// no quote by end of line
			if (!nextLine()) {
				// do we need to throw exception if eof comes before end of string?
				break;
			}
		}
		char t = 0;
		if (_pos < _lineLen) {
			char ch = _lineText[_pos];
			if (ch == 'c' || ch == 'w' || ch == 'd') {
				t = ch;
                _pos++;
                if (_pos < _lineLen) {
                    int nextPos;
                    dchar dch = decodeChar(nextPos);
                    if (isIdentMiddleChar(dch)) {
                        updateTokenText();
                        _token.id = TokId.error_invalidStringSuffix;
                        return;
                    }
                }
            } else {
                int nextPos;
                dchar dch = decodeChar(nextPos);
                if (isIdentMiddleChar(dch)) {
                    updateTokenText();
                    _token.id = TokId.error_invalidStringSuffix;
                    return;
                }
            }
		}
		if (t != 0) {
			if (type != 0 && t != type) {
				//return parserError("Cannot concatenate strings of different type", _sharedStringLiteralToken);
                return;
            }
			type = t;
		}
        updateTokenText();
        _token.setType(TokType.str, type);
        if (_stringTokenMode == StringTokenMode.raw) {
            return;
        }

        _token.str = removeQuotes(_token.str);

		if (wysiwyg || _stringTokenMode == StringTokenMode.rawnoquotes) {
			// no escape processing
			return;
		}
        if (!processEscapeSequences(_token.str)) {
            // error in escape sequence
            _token.id = TokId.error_invalidEscapeSequence;
        }
    }

    /// parse next token
    Tok nextToken() {
        startToken();
        if (_pos >= _lineLen) {
            // whitespace or eof
            parseWhitespace();
            return _token;
        }
        char ch = _lineText[_pos];
        if (ch == 0x0020 || ch == 0x0009 || ch == 0x000B || ch == 0x000C) {
            parseWhitespace();
            return _token;
        }
        char ch2 = _pos + 1 < _lineLen ? _lineText[_pos + 1] : 0;
        char ch3 = _pos + 2 < _lineLen ? _lineText[_pos + 2] : 0;
        if (ch == '/' && ch2 == '/') {
            parseSingleLineComment();
            return _token;
        }
        if (ch == '/' && ch2 == '*') {
            parseMultilineComment();
            return _token;
        }
        if (ch == '/' && ch2 == '+') {
            parseNestedComment();
            return _token;
        }
        if (ch == '\"') {
            parseStringLiteral();
            return _token;
        }
        if (ch == '`') {
            parseStringLiteral();
            return _token;
        }
        if (ch == 'r' && ch2 == '\"') {
            parseStringLiteral();
            return _token;
        }
        OpCode op = detectOp();
        if (op != OpCode.NONE) {
            _token.setType(TokType.op, op);
            updateTokenText();
            return _token;
        }
        int nextPos;
        dchar dch = decodeChar(nextPos);
        if (isIdentStartChar(dch)) {
            _pos = nextPos;
            parseIdentOrKeyword();
            return _token;
        }
        errorToken();
        return _token;
    }
}

enum OpCode : ubyte {
	NONE,       //    no op
	DIV, 		//    /
	DIV_EQ, 	//    /=
	DOT, 		//    .
	DOT_DOT, 	//    ..
	DOT_DOT_DOT,//    ...
	AND, 		//    &
	AND_EQ, 	//    &=
	LOG_AND, 	//    &&
	OR, 		//    |
	OR_EQ, 		//    |=
	LOG_OR, 	//    ||
	MINUS, 		//    -
	MINUS_EQ, 	//    -=
	MINUS_MINUS,//    --
	PLUS, 		//    +
	PLUS_EQ, 	//    +=
	PLUS_PLUS, 	//    ++
	LT, 		//    <
	LT_EQ, 		//    <=
	SHL, 		//    <<
	SHL_EQ, 	//    <<=
	LT_GT, 		//    <>
	NE_EQ, 		//    <>=
	GT, 		//    >
	GT_EQ, 		//    >=
	SHR_EQ,		//    >>=
	ASR_EQ, 	//    >>>=
	SHR, 		//    >>
	ASR, 		//    >>>
	NOT, 		//    !
	NOT_EQ,		//    !=
	NOT_LT_GT, 	//    !<>
	NOT_LT_GT_EQ, //    !<>=
	NOT_LT, 	//    !<
	NOT_LT_EQ, 	//    !<=
	NOT_GT, 	//    !>
	NOT_GT_EQ, 	//    !>=
	PAR_OPEN, 	//    (
	PAR_CLOSE, 	//    )
	SQ_OPEN, 	//    [
	SQ_CLOSE, 	//    ]
	CURL_OPEN, 	//    {
	CURL_CLOSE, //    }
	QUEST, 		//    ?
	COMMA, 		//    ,
	SEMICOLON,  //    ;
	COLON, 	    //    :
	DOLLAR, 	//    $
	EQ, 		//    =
	QE_EQ, 		//    ==
	MUL, 		//    *
	MUL_EQ, 	//    *=
	MOD, 	//    %
	MOD_EQ, //    %=
	XOR, 		//    ^
	XOR_EQ, 	//    ^=
	LOG_XOR, 	//    ^^
	LOG_XOR_EQ, //    ^^=
	INV, 		//    ~
	INV_EQ, 	//    ~=
	AT, 		//    @
	EQ_GT, 		//    =>
	SHARP 		//    #
};

immutable dstring[] OP_CODE_STRINGS = [
	"",
	"/",
	"/=",
	".",
	"..",
	"...",
	"&",
	"&=",
	"&&",
	"|",
	"|=",
	"||",
	"-",
	"-=",
	"--",
	"+",
	"+=",
	"++",
	"<",
	"<=",
	"<<",
	"<<=",
	"<>",
	"<>=",
	">",
	">=",
	">>=",
	">>>=",
	">>",
	">>>",
	"!",
	"!=",
	"!<>",
	"!<>=",
	"!<",
	"!<=",
	"!>",
	"!>=",
	"(",
	")",
	"[",
	"]",
	"{",
	"}",
	"?",
	",",
	";",
	":",
	"$",
	"=",
	"==",
	"*",
	"*=",
	"%",
	"%=",
	"^",
	"^=",
	"^^",
	"^^=",
	"~",
	"~=",
	"@",
	"=>",
	"#"
];

dstring getOpNameD(OpCode op) pure nothrow {
	return OP_CODE_STRINGS[op];
};

enum Keyword : ubyte {
	NONE,
	ABSTRACT,
	ALIAS,
	ALIGN,
	ASM,
	ASSERT,
	AUTO,

	BODY,
	BOOL,
	BREAK,
	BYTE,

	CASE,
	CAST,
	CATCH,
	CDOUBLE,
	CENT,
	CFLOAT,
	CHAR,
	CLASS,
	CONST,
	CONTINUE,
	CREAL,

	DCHAR,
	DEBUG,
	DEFAULT,
	DELEGATE,
	DELETE,
	DEPRECATED,
	DO,
	DOUBLE,

	ELSE,
	ENUM,
	EXPORT,
	EXTERN,

	FALSE,
	FINAL,
	FINALLY,
	FLOAT,
	FOR,
	FOREACH,
	FOREACH_REVERSE,
	FUNCTION,

	GOTO,

	IDOUBLE,
	IF,
	IFLOAT,
	IMMUTABLE,
	IMPORT,
	IN,
	INOUT,
	INT,
	INTERFACE,
	INVARIANT,
	IREAL,
	IS,

	LAZY,
	LONG,

	MACRO,
	MIXIN,
	MODULE,

	NEW,
	NOTHROW,
	NULL,

	OUT,
	OVERRIDE,

	PACKAGE,
	PRAGMA,
	PRIVATE,
	PROTECTED,
	PUBLIC,
	PURE,

	REAL,
	REF,
	RETURN,

	SCOPE,
	SHARED,
	SHORT,
	STATIC,
	STRUCT,
	SUPER,
	SWITCH,
	SYNCHRONIZED,

	TEMPLATE,
	THIS,
	THROW,
	TRUE,
	TRY,
	TYPEDEF,
	TYPEID,
	TYPEOF,

	UBYTE,
	UCENT,
	UINT,
	ULONG,
	UNION,
	UNITTEST,
	USHORT,

	VERSION,
	VOID,
	VOLATILE,

	WCHAR,
	WHILE,
	WITH,

	FILE,
	MODULE__,
	LINE,
	FUNCTION__,
	PRETTY_FUNCTION,

	//Special Token	Replaced with
	DATE, //	string literal of the date of compilation "mmm dd yyyy"
	EOF, //	sets the scanner to the end of the file
	TIME, //	string literal of the time of compilation "hh:mm:ss"
	TIMESTAMP, //	string literal of the date and time of compilation "www mmm dd hh:mm:ss yyyy"
	VENDOR, //	Compiler vendor string, such as "Digital Mars D"
	VERSION_, //	Compiler version as an integer, such as 2001
	
	GSHARED,
	TRAITS,
	VECTOR,
	PARAMETERS,

}

immutable string[] KEYWORD_STRINGS = [
	"",
	"abstract",
	"alias",
	"align",
	"asm",
	"assert",
	"auto",

	"body",
	"bool",
	"break",
	"byte",

	"case",
	"cast",
	"catch",
	"cdouble",
	"cent",
	"cfloat",
	"char",
	"class",
	"const",
	"continue",
	"creal",

	"dchar",
	"debug",
	"default",
	"delegate",
	"delete",
	"deprecated",
	"do",
	"double",

	"else",
	"enum",
	"export",
	"extern",

	"false",
	"final",
	"finally",
	"float",
	"for",
	"foreach",
	"foreach_reverse",
	"function",

	"goto",

	"idouble",
	"if",
	"ifloat",
	"immutable",
	"import",
	"in",
	"inout", 
	"int",
	"interface",
	"invariant",
	"ireal",
	"is",

	"lazy",
	"long",

	"macro",
	"mixin",
	"module",

	"new",
	"nothrow",
	"null",

	"out",
	"override",

	"package",
	"pragma",
	"private",
	"protected",
	"public",
	"pure",

	"real",
	"ref",
	"return",

	"scope",
	"shared",
	"short",
	"static",
	"struct",
	"super",
	"switch",
	"synchronized",

	"template",
	"this",
	"throw",
	"true",
	"try",
	"typedef",
	"typeid",
	"typeof",

	"ubyte",
	"ucent",
	"uint",
	"ulong",
	"union",
	"unittest",
	"ushort",

	"version",
	"void",
	"volatile",

	"wchar",
	"while",
	"with",

	"__FILE__",
	"__MODULE__",
	"__LINE__",
	"__FUNCTION__",
	"__PRETTY_FUNCTION__",

	//Special Token	Replaced with
	"__DATE__", //	string literal of the date of compilation "mmm dd yyyy"
	"__EOF__", //	sets the scanner to the end of the file
	"__TIME__", //	string literal of the time of compilation "hh:mm:ss"
	"__TIMESTAMP__", //	string literal of the date and time of compilation "www mmm dd hh:mm:ss yyyy"
	"__VENDOR__", //	Compiler vendor string, such as "Digital Mars D"
	"__VERSION__", //	Compiler version as an integer, such as 2001

		
	"__gshared",
	"__traits",
	"__vector",
	"__parameters"
];

public string getKeywordName(Keyword keyword) pure nothrow {
	return KEYWORD_STRINGS[keyword];
};

public dstring getKeywordNameD(Keyword keyword) {
	return toUTF32(KEYWORD_STRINGS[keyword]);
};

public Keyword findKeyword(Keyword start, Keyword end, dchar * name, int len, ref int pos) pure nothrow {
	for (Keyword i = start; i <= end; i++) {
		string s = KEYWORD_STRINGS[i];
		if (s.length > len + 1)
			continue; // too long
		bool found = true;
		for (uint j = 1; j < s.length; j++) {
			if (s[j] != name[j - 1]) {
				found = false;
				break;
			}
		}
		if (found) {
			if (s.length == len - 1 || !isIdentMiddleChar(name[s.length - 1])) {
				pos += s.length - 1;
				return i;
			}
		}
	}
	return Keyword.NONE;
}

public Keyword findKeyword(Keyword start, Keyword end, string ident) pure nothrow {
	for (Keyword i = start; i <= end; i++) {
		string s = KEYWORD_STRINGS[i];
        if (s.equal(ident))
            return i;
	}
	return Keyword.NONE;
}

public Keyword findKeyword(string ident) {
    char ch = ident[0];
	if (ch > 'z')
		return Keyword.NONE;
	switch (cast(ubyte)ch) {
		//	ABSTRACT,
		//	ALIAS,
		//	ALIGN,
		//	ASM,
		//	ASSERT,
		//	AUTO,
		case 'a': return findKeyword(Keyword.ABSTRACT, Keyword.AUTO, ident);

		//	BODY,
		//	BOOL,
		//	BREAK,
		//	BYTE,
		case 'b': return findKeyword(Keyword.BODY, Keyword.BYTE, ident);
				
		//	CASE,
		//	CAST,
		//	CATCH,
		//	CDOUBLE,
		//	CENT,
		//	CFLOAT,
		//	CHAR,
		//	CLASS,
		//	CONST,
		//	CONTINUE,
		//	CREAL,
		case 'c': return findKeyword(Keyword.CASE, Keyword.CREAL, ident);
				
		//	DCHAR,
		//	DEBUG,
		//	DEFAULT,
		//	DELEGATE,
		//	DELETE,
		//	DEPRECATED,
		//	DO,
		//	DOUBLE,
		case 'd': return findKeyword(Keyword.DCHAR, Keyword.DOUBLE, ident);
				
		//	ELSE,
		//	ENUM,
		//	EXPORT,
		//	EXTERN,
		case 'e': return findKeyword(Keyword.ELSE, Keyword.EXTERN, ident);
				
		//	FALSE,
		//	FINAL,
		//	FINALLY,
		//	FLOAT,
		//	FOR,
		//	FOREACH,
		//	FOREACH_REVERSE,
		//	FUNCTION,
		case 'f': return findKeyword(Keyword.FALSE, Keyword.FUNCTION, ident);
				
		//	GOTO,
		case 'g': return findKeyword(Keyword.GOTO, Keyword.GOTO, ident);
				
		//	IDOUBLE,
		//	IF,
		//	IFLOAT,
		//	IMMUTABLE,
		//	IMPORT,
		//	IN,
		//	INOUT,
		//	INT,
		//	INTERFACE,
		//	INVARIANT,
		//	IREAL,
		//	IS,
		case 'i': return findKeyword(Keyword.IDOUBLE, Keyword.IS, ident);
				
		//	LAZY,
		//	LONG,
		case 'l': return findKeyword(Keyword.LAZY, Keyword.LONG, ident);
				
		//	MACRO,
		//	MIXIN,
		//	MODULE,
		case 'm': return findKeyword(Keyword.MACRO, Keyword.MODULE, ident);
				
		//	NEW,
		//	NOTHROW,
		//	NULL,
		case 'n': return findKeyword(Keyword.NEW, Keyword.NULL, ident);
				
		//	OUT,
		//	OVERRIDE,
		case 'o': return findKeyword(Keyword.OUT, Keyword.OVERRIDE, ident);
				
		//	PACKAGE,
		//	PRAGMA,
		//	PRIVATE,
		//	PROTECTED,
		//	PUBLIC,
		//	PURE,
		case 'p': return findKeyword(Keyword.PACKAGE, Keyword.PURE, ident);
				
		//	REAL,
		//	REF,
		//	RETURN,
		case 'r': return findKeyword(Keyword.REAL, Keyword.RETURN, ident);
				
		//	SCOPE,
		//	SHARED,
		//	SHORT,
		//	STATIC,
		//	STRUCT,
		//	SUPER,
		//	SWITCH,
		//	SYNCHRONIZED,
		case 's': return findKeyword(Keyword.SCOPE, Keyword.SYNCHRONIZED, ident);
				
		//	TEMPLATE,
		//	THIS,
		//	THROW,
		//	TRUE,
		//	TRY,
		//	TYPEDEF,
		//	TYPEID,
		//	TYPEOF,
		case 't': return findKeyword(Keyword.TEMPLATE, Keyword.TYPEOF, ident);
				
		//	UBYTE,
		//	UCENT,
		//	UINT,
		//	ULONG,
		//	UNION,
		//	UNITTEST,
		//	USHORT,
		case 'u': return findKeyword(Keyword.UBYTE, Keyword.USHORT, ident);
				
		//	VERSION,
		//	VOID,
		//	VOLATILE,
		case 'v': return findKeyword(Keyword.VERSION, Keyword.VOLATILE, ident);
				
		//	WCHAR,
		//	WHILE,
		//	WITH,
		case 'w': return findKeyword(Keyword.WCHAR, Keyword.WITH, ident);
				
		//	FILE,
		//	MODULE,
		//	LINE,
		//	FUNCTION,
		//	PRETTY_FUNCTION,
		//
		//	GSHARED,
		//	TRAITS,
		//	VECTOR,
		//	PARAMETERS,
		case '_': return findKeyword(Keyword.FILE, Keyword.PARAMETERS, ident);
		default: return Keyword.NONE;				
	}
}	

/**
 * Token.
 */
class Token {
	protected SourceFile _file;
	protected int _line;
	protected int _pos;
	protected TokenType _type;
    /// returns token type
	@property TokenType type() { return _type; }
    /// returns file info for source
	@property SourceFile filename() { return _file; }
    /// returns 1-based source line number of token start
	@property int line() { return _line; }
    /// returns 1-based source line position of token start
	@property int pos() { return _pos; }
    /// returns token text
	@property dchar[] text() { return null; }

    // number token properties
	@property dchar literalType() { return 0; }
	@property ulong intValue() { return 0; }
	@property bool isUnsigned() { return false; }
	@property ulong isLong() { return false; }
	@property real realValue() { return 0; }
	@property double doubleValue() { return 0; }
	@property float floatValue() { return 0; }
	@property byte precision() { return 0; }
	@property bool isImaginary() { return false; }

    /// returns opcode ID - for opcode tokens
	@property OpCode opCode() { return OpCode.NONE; }
    /// returns keyword ID - for keyword tokens
	@property Keyword keyword() { return Keyword.NONE; }
    /// returns true if this is documentation comment token
    @property bool isDocumentationComment() { return false; }
    /// returns true if this is multiline
    @property bool isMultilineComment() { return false; }

    // error handling

    /// returns true if it's invalid token (can be returned in error tolerant mode of tokenizer)
    @property bool isError() { return type == TokenType.INVALID; }
    /// returns error message if it's invalid token (can be returned in error tolerant mode of tokenizer)
    @property string errorMessage() { return null; }
    /// returns error code if it's invalid token (can be returned in error tolerant mode of tokenizer)
    @property int errorCode() { return 0; }
    /// returns type of token parsing of which has been failed - if it's invalid token (can be returned in error tolerant mode of tokenizer)
    @property TokenType invalidTokenType() { return TokenType.INVALID; }


	this(TokenType type) {
		_type = type;
	}

	this(TokenType type, SourceFile file, int line, int pos) {
		_type = type;
		_file = file;
		_line = line;
		_pos = pos;
	}
    /// set start position for token (line is 1-based, pos is 0-based)
	void setPos(SourceFile file, int line, int pos) {
		_file = file;
		_line = line;
		_pos = pos + 1;
	}
    /// set source file information for token
	void setFile(SourceFile file) {
		_file = file;
	}
    /// set start position for token (line is 1-based, pos is 0-based)
	void setPos(int line, int pos) {
		_line = line;
		_pos = pos + 1;
	}

	public abstract Token clone();
	public override @property string toString() {
		return "" ~ to!string(_line) ~ ":" ~ to!string(_pos) ~ " " ~ to!string(type) ~ " " ~ to!string(opCode) ~ " " ~ to!string(keyword) 
			~" \"" ~ toUTF8(text()) ~ "\"";
	}
}

class EofToken : Token {
	this() {
		super(TokenType.EOF);
	}
	this(SourceFile file, uint line, uint pos) {
		super(TokenType.EOF, file, line, pos);
	}
	override public Token clone() {
		return new EofToken(_file, _line, _pos);
	}
	public override @property string toString() {
		return "EOF";
	}
}

// treat as white space
//class EolToken : Token {
//	this(string file, uint line, uint pos) {
//		super(TokenType.EOL, file, line, pos);
//	}
//}

/// white space token
class WhiteSpaceToken : Token {
	this() {
		super(TokenType.WHITESPACE);
	}
	this(SourceFile file, uint line, uint pos) {
		super(TokenType.WHITESPACE, file, line, pos);
	}
	override public Token clone() {
		return new WhiteSpaceToken(_file, _line, _pos);
	}
	public override @property string toString() {
		return "WhiteSpace";
	}
}

class OpToken : Token {
	OpCode _op;
	public @property override OpCode opCode() { return _op; }
	public @property void opCode(OpCode op) { _op = op; }
	public @property override dchar[] text() { return cast(dchar[])getOpNameD(_op); }
	this() {
		super(TokenType.OP);
	}
	this(SourceFile file, uint line, uint pos) {
		super(TokenType.OP, file, line, pos);
	}
	override public Token clone() {
		OpToken res = new OpToken(_file, _line, _pos);
        res._op = _op;
        return res;
	}
	public override @property string toString() {
		return "Op:" ~ to!string(_op);
	}
}

class KeywordToken : Token {
	Keyword _keyword;
	public @property override Keyword keyword() { return _keyword; }
	public @property void keyword(Keyword keyword) { _keyword = keyword; }
	public @property override dchar[] text() { return cast(dchar[])getKeywordNameD(_keyword); }
	this() {
		super(TokenType.KEYWORD);
	}
	this(SourceFile file, uint line, uint pos) {
		super(TokenType.KEYWORD, file, line, pos);
	}
	override public Token clone() {
		KeywordToken res = new KeywordToken(_file, _line, _pos);
        res._keyword = _keyword;
        return res;
	}
	public override @property string toString() {
		return "Keyword:" ~ to!string(_keyword);
	}
}

/// comment token
class CommentToken : Token {
	protected dchar[] _text;
    protected bool _isDocumentationComment;
    protected bool _isMultilineComment;


    override @property bool isDocumentationComment() {
        return _isDocumentationComment;
    }

    @property void isDocumentationComment(bool f) {
        _isDocumentationComment = f;
    }

    /// returns true if this is multiline
    override @property bool isMultilineComment() {
        return _isMultilineComment;
    }

    @property void isMultilineComment(bool f) {
        _isMultilineComment = f;
    }

	@property override dchar[] text() { return _text; }
	@property void text(dchar[] text) { _text = text; }
	this() {
		super(TokenType.COMMENT);
	}
	this(SourceFile file, uint line, uint pos, dchar[] text) {
		super(TokenType.COMMENT, file, line, pos);
		_text = text;
	}
	override public Token clone() {
		CommentToken res = new CommentToken(_file, _line, _pos, _text.dup);
        res._isDocumentationComment = _isDocumentationComment;
        res._isMultilineComment = _isMultilineComment;
        return res;
	}
	public override @property string toString() {
		return "Comment:" ~ to!string(_text);
	}
}

/// Invalid token holder - for error tolerant parsing
class InvalidToken : Token {
	protected dchar[] _text;
    protected TokenType _invalidTokenType;
    protected int _errorCode;
    protected string _errorMessage;

    /// returns error message if it's invalid token (can be returned in error tolerant mode of tokenizer)
    override @property string errorMessage() { return _errorMessage; }
    /// sets error message
    @property void errorMessage(string s) { _errorMessage = s; }
    /// returns error code if it's invalid token (can be returned in error tolerant mode of tokenizer)
    override @property int errorCode() { return _errorCode; }
    /// sets error code
    @property void errorCode(int c) { _errorCode = c; }
    /// returns type of token parsing of which has been failed - if it's invalid token (can be returned in error tolerant mode of tokenizer)
    override @property TokenType invalidTokenType() { return _invalidTokenType; }
    /// sets type of token parsing of which has been failed
    @property void invalidTokenType(TokenType t) { _invalidTokenType = t; }

    /// text of invalid token
	@property override dchar[] text() { return _text; }
    /// text of invalid token
	@property void text(dchar[] text) { _text = text; }

	this() {
		super(TokenType.INVALID);
	}
	this(SourceFile file, uint line, uint pos, dchar[] text) {
		super(TokenType.INVALID, file, line, pos);
		_text = text;
	}
	override Token clone() {
		InvalidToken res = new InvalidToken(_file, _line, _pos, _text.dup);
        res._errorMessage = _errorMessage.dup;
        res._errorCode = _errorCode;
        res._invalidTokenType = _invalidTokenType;
        return res;
	}
	override @property string toString() {
		return "Invalid:" ~ to!string(_text);
	}
}

alias tokenizer_ident_t = uint;
alias tokenizer_ident_name_t = dchar[];

enum : tokenizer_ident_t {
    NO_IDENT = 0
}

/**
 * Global storage for identifier strings.
 */
class IdentHolder {
    protected tokenizer_ident_t _nextId;
    protected tokenizer_ident_name_t[tokenizer_ident_t] _idToName;
    protected tokenizer_ident_t[tokenizer_ident_name_t] _nameToId;

    public this() {
        _nextId = NO_IDENT + 1;
    }

    /**
    * Search for id by name, return NO_IDENT if not found.
    */
    uint findByName(tokenizer_ident_name_t name) {
        tokenizer_ident_t * found = (name in _nameToId);
        if (found)
            return *found; 
        return NO_IDENT;
    }

    /**
    * Search for name by id, return null if not found.
    */
    tokenizer_ident_name_t nameById(tokenizer_ident_t id) {
        auto found = (id in _idToName);
        if (found)
            return *found;
        return null;
    }

    /**
     * Search for ident id by name, create new entry if not found.
     */
    tokenizer_ident_t idByName(tokenizer_ident_name_t name) {
        uint * found = (name in _nameToId);
        if (found)
            return *found; 
        uint newid = _nextId++;
        _nameToId[cast(dstring)name] = newid;
        _idToName[newid] = cast(tokenizer_ident_name_t)name;
        return newid;
    }
}

/**
* Thread local storage for IDs.
*/
IdentHolder identMap;

static this() {
    // init ID storage
    identMap = new IdentHolder();
}

class StringLiteralToken : Token {
	dchar[] _text;
	dchar _literalType;
	public @property override dchar literalType() { return _literalType; }
	public @property override dchar[] text() { return _text; }
	public void setText(dchar[] text, dchar type) { _text = text; _literalType = type; }
	this() {
		super(TokenType.STRING);
	}
	this(SourceFile file, uint line, uint pos, dchar[] text, dchar type) {
		super(TokenType.STRING, file, line, pos);
		_text = text;
		_literalType = type;
	}
	override public Token clone() {
		return new StringLiteralToken(_file, _line, _pos, _text.dup, _literalType);
	}
	public override @property string toString() {
        return toUTF8("String:\"" ~ _text ~ "\"" ~ (_literalType ? _literalType : ' '));
	}
}

class CharacterLiteralToken : Token {
	dchar _character;
	dchar _literalType;
	@property override dchar literalType() { return _literalType; }
    @property dchar character() { return _character; }
	@property override dchar[] text() { return [_character]; }
	void setCharacter(dchar ch, dchar type) { _character = ch; _literalType = type; }
	this() {
		super(TokenType.CHARACTER);
	}
	this(SourceFile file, uint line, uint pos, dchar character, dchar type) {
		super(TokenType.CHARACTER, file, line, pos);
		_character = character;
		_literalType = type;
	}
	override public Token clone() {
		return new CharacterLiteralToken(_file, _line, _pos, _character, _literalType);
	}
	public override @property string toString() {
		return "Char:" ~ toUTF8([_character]);
	}
}

class IntegerLiteralToken : Token {
	ulong _value;
	bool _unsigned;
	bool _long;
	public @property override ulong intValue() { return _value; }
	public @property override bool isUnsigned() { return _unsigned; }
	public @property override ulong isLong() { return _long; }
	public @property override dchar[] text() { return cast(dchar[])to!dstring(_value); }
	public void setValue(ulong value, bool unsignedFlag = false, bool longFlag = false) {
		_value = value;
		_unsigned = unsignedFlag;
		_long = longFlag;
	}
	public void setFlags(bool unsignedFlag = false, bool longFlag = false) {
		_unsigned = unsignedFlag;
		_long = longFlag;
	}
	this() {
		super(TokenType.INTEGER);
	}
	this(SourceFile file, uint line, uint pos, ulong value, bool unsignedFlag, bool longFlag) {
		super(TokenType.INTEGER, file, line, pos);
		_value = value;
		_unsigned = unsignedFlag;
		_long = longFlag;
	}
	override public Token clone() {
		return new IntegerLiteralToken(_file, _line, _pos, _value, _unsigned, _long);
	}
	public override @property string toString() {
		return "Integer:" ~ to!string(_value) ~ (_long ? "L" : "") ~ (_unsigned ? "U" : "");
	}
}

class RealLiteralToken : Token {
	real _value;
	byte _precision;
	bool _imaginary;
	public @property override ulong intValue() { return to!long(_value); }
	public @property override real realValue() { return _value; }
	public @property override double doubleValue() { return cast(double)_value; }
	public @property override float floatValue() { return cast(float)_value; }
	public @property override byte precision() { return _precision; }
	public @property override bool isImaginary() { return _imaginary; }
	public @property override dchar[] text() { return cast(dchar[])to!dstring(_value); }
	public void setValue(real value, byte precision = 1, bool imaginary = false) {
		_value = value;
		_precision = precision;
		_imaginary = imaginary;
	}
	public void setFlags(byte precision = 1, bool imaginary = false) {
		_precision = precision;
		_imaginary = imaginary;
	}
	this() {
		super(TokenType.FLOAT);
	}
	this(SourceFile file, uint line, uint pos, real value, byte precision, bool imaginary) {
		super(TokenType.FLOAT, file, line, pos);
		_value = value;
		_precision = precision;
		_imaginary = imaginary;
	}
	override public Token clone() {
		return new RealLiteralToken(_file, _line, _pos, _value, _precision, _imaginary);
	}
	public override @property string toString() {
		return "Real:" ~ to!string(_value) ~ (_precision == 0 ? "f" : (_precision == 2 ? "L" : "")) ~ (_imaginary ? "i" : "");
	}
}

class IdentToken : Token {
	tokenizer_ident_t _id;
	public @property override dchar[] text() { return identMap.nameById(_id); }
	public void setText(dchar[] text) { _id = identMap.idByName(text); }
	this() {
		super(TokenType.IDENTIFIER);
	}
	this(SourceFile file, uint line, uint pos, dchar[] text) {
		super(TokenType.IDENTIFIER, file, line, pos);
		_id = identMap.idByName(text);
	}
	this(SourceFile file, uint line, uint pos, tokenizer_ident_t id) {
		super(TokenType.IDENTIFIER, file, line, pos);
		_id = id;
	}
	override public Token clone() {
		return new IdentToken(_file, _line, _pos, _id);
	}
	public override @property string toString() {
		return "Ident:" ~ to!string(text);
	}
}

// shared appender buffer, to avoid extra heap allocations
struct StringAppender {
	dchar[] buf;
	uint len;
	dchar[] get() {
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
	void append(dchar[] s) {
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
	void append(dchar ch) {
		if (len + 1 > buf.length) {
			uint newsize = cast(uint)(buf.length * 2);
			if (newsize < 128)
				newsize = 128;
			buf.length = newsize;
		}
		buf[len++] = ch;
	}
	void reset() {
		len = 0;
	}
    static int parseHexDigit(dchar ch) {
        if (ch >= '0' && ch <='9')
            return ch - '0';
        if (ch >= 'a' && ch <='f')
            return ch - 'a' + 10;
        if (ch >= 'A' && ch <='F')
            return ch - 'A' + 10;
        return -1;
    }
    bool errorFlag = false;
    dchar decodeHex(ref int pos, int count) {
        dchar res = 0;
        for (int i = 0; i < count; i++) {
            if (pos >= len - 1) {
                errorFlag = true;
                return res;
            }
            dchar ch = buf[++pos];
            int digit = parseHexDigit(ch);
            if (digit < 0) {
                errorFlag = true;
                digit = 0;
            }
            res = (res << 4) | digit;
        }
        return res;
    }
    dchar decodeOct(dchar firstChar, ref int pos) {
        dchar res = 0;
        res = firstChar - '0';
        if (pos < len - 1 && buf[pos + 1] >= '0' && buf[pos + 1] <= '7') {
            res = (res << 3) | (buf[++pos] - '0');
        }
        if (pos < len - 1 && buf[pos + 1] >= '0' && buf[pos + 1] <= '7') {
            res = (res << 3) | (buf[++pos] - '0');
        }
        return res;
    }

    char[] entityNameBuf;
    int entityNameLen;

    dchar decodeCharacterEntity(ref int pos) {
        entityNameLen = 0;
        pos++;
        for(; pos < len && buf[pos] != ';'; pos++) {
            dchar ch = buf[pos];
            if (ch >= 0x80)
                errorFlag = true;
            if (entityNameBuf.length < entityNameLen + 4)
                entityNameBuf.length += 32;
            entityNameBuf[entityNameLen++] = cast(char)ch;
        }
        if (pos < len && buf[pos] == ';') {
            dchar ch = entityToChar(cast(string)entityNameBuf[0 .. entityNameLen]);
            if (ch)
                return ch;
        }
        errorFlag = true;
        return '?';
    }

    bool processEscapeSequences() {
        errorFlag = false;
        int dst = 0;
        for (int src = 0; src < len; src++) {
            dchar ch = buf[src];
            if (ch == '\\') {
                if (src == len - 1)
                    break; // INVALID
                ch = buf[++src];
                switch (ch) {
                    case '\'':
                    case '\"':
                    case '?':
                    case '\\':
                        buf[dst++] = ch;
                        break;
                    case '0':
                        buf[dst++] = '\0';
                        break;
                    case 'a':
                        buf[dst++] = '\a';
                        break;
                    case 'b':
                        buf[dst++] = '\b';
                        break;
                    case 'f':
                        buf[dst++] = '\f';
                        break;
                    case 'n':
                        buf[dst++] = '\n';
                        break;
                    case 'r':
                        buf[dst++] = '\r';
                        break;
                    case 't':
                        buf[dst++] = '\t';
                        break;
                    case 'v':
                        buf[dst++] = '\v';
                        break;
                    case 'x':
                        buf[dst++] = decodeHex(src, 2);
                        break;
                    case 'u':
                        buf[dst++] = decodeHex(src, 4);
                        break;
                    case 'U':
                        buf[dst++] = decodeHex(src, 8);
                        break;
                    default:
                        if (ch >= '0' && ch <= '7') {
                            // octal X XX or XXX
                            buf[dst++] = decodeOct(ch, src); // something wrong
                        } else if (ch == '&') {
                            // named character entity
                            buf[dst++] = decodeCharacterEntity(src);
                            // just show it as is
                        } else {
                            buf[dst++] = ch; // something wrong
                            errorFlag = true;
                        }
                        break;
                }
            } else {
                buf[dst++] = ch;
            }
        }
        len = dst;
        return errorFlag;
    }
}

class Tokenizer
{
	protected SourceLines _lineStream;
	protected dchar[] _lineText;
	protected int _line; // current line number
	protected int _len; // current line length
	protected int _pos; // current line read position
    protected int _prevLineLength; // previous line length
	protected uint _state; // tokenizer state
	
	enum : int {
		EOF_CHAR = 0x001A,
		EOL_CHAR = 0x000A
	};
	
	protected WhiteSpaceToken _sharedWhiteSpaceToken = new WhiteSpaceToken();
	protected CommentToken _sharedCommentToken = new CommentToken();
	protected StringLiteralToken _sharedStringLiteralToken = new StringLiteralToken();
	protected IdentToken _sharedIdentToken = new IdentToken();
	protected OpToken _sharedOpToken = new OpToken();
	protected KeywordToken _sharedKeywordToken = new KeywordToken();
	protected IntegerLiteralToken _sharedIntegerToken = new IntegerLiteralToken();
	protected RealLiteralToken _sharedRealToken = new RealLiteralToken();
    protected InvalidToken _sharedInvalidToken = new InvalidToken();
    protected CharacterLiteralToken _sharedCharacterLiteralToken = new CharacterLiteralToken();
	protected StringAppender _stringLiteralAppender;
	protected StringAppender _commentAppender;
	protected StringAppender _identAppender;
	
	protected bool _enableCommentText = true;
    /// when false, does not put comment text into comment token - for less allocations
	@property void enableCommentText(bool enabled) {
		_enableCommentText = enabled;
	}
    /// when false, does not put comment text into comment token - for less allocations
	@property bool enableCommentText() {
		return _enableCommentText;
	}

	protected bool _errorTolerant = false;
    /// when true, returns BadToken instead of throwing exception
	@property void errorTolerant(bool enabled) {
		_errorTolerant = enabled;
	}
    /// when true, returns BadToken instead of throwing exception
	@property bool errorTolerant() {
		return _errorTolerant;
	}

	this(SourceLines lineStream) {
        init(lineStream);
	}

    void init(SourceLines lineStream, int pos = 0) {
		_lineStream = lineStream;
        SourceFile file = _lineStream.file;
		_sharedWhiteSpaceToken.setFile(file);
		_sharedCommentToken.setFile(file);
		_sharedStringLiteralToken.setFile(file);
		_sharedIdentToken.setFile(file);
		_sharedOpToken.setFile(file);
		_sharedKeywordToken.setFile(file);
		_sharedIntegerToken.setFile(file);
		_sharedRealToken.setFile(file);
        _sharedInvalidToken.setFile(file);
        _sharedCharacterLiteralToken.setFile(file);
		buildTime = Clock.currTime();
        _line = lineStream.line;
        _pos = 0;
        _prevLineLength = 0;
        _lineText = null;
        nextLine();
        _pos = pos;
    }
	
	this(string code, string filename = "") {
		this(new ArraySourceLines(code, filename));
	}
	
	// fetch next line from source stream
	protected bool nextLine() {
        _prevLineLength = cast(int)_lineText.length;
		_lineText = _lineStream.readLine();
		if (!_lineText) {
			if (_lineStream.errorCode != 0)
				throw new SourceEncodingException(_lineStream.errorMessage, _lineStream.file, _lineStream.errorLine, _lineStream.errorPos);
            if (_lineStream.eof) {
                // end of file
                _pos = 0;
			    _len = 0;
			    return false;
            }
            // just an empty line
		}
		_line = _lineStream.line;
		_pos = 0;
		_len = cast(int)_lineText.length; // do not support lines longer that 4Gb
		return true;
	}
	
	protected dchar nextChar() {
	    if (_pos >= _len) {
			if (!nextLine()) {
                _pos = _prevLineLength + 1;
				return EOF_CHAR;
			}
			return EOL_CHAR;
		}
		dchar res = _lineText[_pos++];
        if (_pos >= _len)
            nextLine();
        return res;
	}
	
	protected dchar peekChar() {
		if (_lineText is null) {
			if (!nextLine()) {
				return EOF_CHAR;
			}
		}
		if (_pos >= _len)
			return EOL_CHAR;
		return _lineText[_pos++];
	}
	
	protected Token emitEof() {
		// TODO: check for current state
		return new EofToken(_lineStream.file, _startLine, _startPos + 2);
	}
	
	protected Token processWhiteSpace(dchar firstChar) {
		// reuse the same token instance, to avoid extra heap spamming
        _sharedWhiteSpaceToken.setPos(_startLine, _startPos);
		for (;;) {
			int i = _pos;
			for (; i < _len; i++) {
				dchar ch = _lineText[i];
				if (!(ch == 0x0020 || ch == 0x0009 || ch == 0x000B || ch == 0x000C || ch == EOL_CHAR))
					break;
			}
			_pos = i;
			if (_pos < _len)
				break;
			// go to next line
			if (!nextLine())
				break;
		}
		return _sharedWhiteSpaceToken;
	}
	
	protected Token processOneLineComment() {
		_sharedCommentToken.setPos(_startLine, _startPos);
        _sharedCommentToken.isDocumentationComment = _pos + 1 < _lineText.length && _lineText[_pos + 1] == '/';
        _sharedCommentToken.isMultilineComment = false;
		if (_enableCommentText) {
			_sharedCommentToken.text = _lineText[_pos + 1 .. $];
		}
		_pos = _len;
        nextChar();
		return _sharedCommentToken;
	}

	protected Token processOneLineSharpComment() {
		_sharedCommentToken.setPos(_startLine, _startPos);
		if (_enableCommentText) {
			_sharedCommentToken.text = _lineText[_pos .. $];
		}
		_pos = _len;
		return _sharedCommentToken;
	}

	// Comment /*   */	
	protected Token processMultilineComment() {
		_sharedCommentToken.setPos(_startLine, _startPos);
        _sharedCommentToken.isDocumentationComment = _pos + 1 < _lineText.length && _lineText[_pos + 1] == '*';
        _sharedCommentToken.isMultilineComment = true;
		_commentAppender.reset();
		int textStart = _pos + 1;
		for (;;) {
			int textEnd = int.max;
			int i = textStart;
			for (; i < _len - 1; i++) {
				if (_lineText[i] == '*' && _lineText[i + 1] == '/') {
					textEnd = i;
					break;
				}
			}
			if (textEnd != int.max) {
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
	
	// Comment /+   +/	
	protected Token processNestedComment() {
		_sharedCommentToken.setPos(_startLine, _startPos);
        _sharedCommentToken.isDocumentationComment = _pos + 1 < _lineText.length && _lineText[_pos + 1] == '+';
        _sharedCommentToken.isMultilineComment = true;
		_commentAppender.reset();
		dchar[] text;
		int textStart = _pos + 1;
		int level = 1;
		for (;;) {
			int textEnd = int.max;
			int i = textStart;
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
			if (textEnd != int.max) {
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
	
	protected Token processHexString() {
		_pos++;
		// TODO:
		return null;
	}
	
	protected Token processDelimitedString() {
		_pos++;
		// TODO:
		return null;
	}
	
	// r"string"   or    `string`
	protected Token processWysiwygString(dchar ch) {
		_pos++;
		// TODO:
		return null;
	}
	
	protected Token processIdent(dchar firstChar) {
		_sharedIdentToken.setPos(_startLine, _startPos);
		_identAppender.reset();
		_identAppender.append(firstChar);
		for (; _pos < _len; ) {
			dchar ch = _lineText[_pos];
			if (!isIdentMiddleChar(ch)) {
				break;
			}
			_identAppender.append(ch);
			_pos++;
		}
		_sharedIdentToken.setText(_identAppender.get);
		return _sharedIdentToken;
	}

	protected Token processIntegerSuffix() {
		if (_pos >= _len)
			return _sharedIntegerToken;
		bool longFlag = false;
		bool unsignedFlag = false;
		dchar ch = _lineText[_pos];
		dchar ch2 = _pos < _len - 1 ? _lineText[_pos + 1] : 0;
		if (ch == 'l' || ch == 'L') {
			longFlag = true;
			_pos++;
			if (ch2 == 'u' || ch2 == 'U') {
				unsignedFlag = true;
				_pos++;
			} 
		} else if (ch == 'u' || ch == 'U') {
			unsignedFlag = true;
			_pos++;
			if (ch2 == 'l' || ch2 == 'L') {
				longFlag = true;
				_pos++;
			} 
		}
		_sharedIntegerToken.setFlags(unsignedFlag, longFlag);
		ch = _pos < _len ? _lineText[_pos] : 0;
		if (isIdentMiddleChar(ch))
			return parserError("Unexpected character after number", _sharedIntegerToken);
		return _sharedIntegerToken;
	}
	
	protected Token processBinaryNumber() {
		_sharedIntegerToken.setPos(_startLine, _startPos);
		_pos++;
		if (_pos >= _len)
			return parserError("Unexpected end of line in binary number", _sharedIntegerToken);
		int digits = 0;
		ulong number = 0;
		int i = _pos;
		for (;i < _len; i++) {
			dchar ch = _lineText[i];
			if (ch != '0' && ch != '1')
				break;
			number = (number << 1) | (ch == '1' ? 1 : 0);
			digits++;
		}
		_pos = i;
		if (digits > 64)
			return parserError("number is too big", _sharedIntegerToken);
		_sharedIntegerToken.setValue(number);
		return processIntegerSuffix();
	}

	protected Token processHexNumber() {
		_sharedIntegerToken.setPos(_startLine, _startPos);
		_sharedRealToken.setPos(_startLine, _startPos);
		_pos++;
		if (_pos >= _len)
			return parserError("Unexpected end of line in hex number", _sharedIntegerToken);
		int digits = 0;
		ulong number = 0;
		int i = _pos;
		for (;i < _len; i++) {
			dchar ch = _lineText[i];
			uint digit = 0;
			if (ch >= '0' && ch <= '9')
				digit = ch - '0';
			else if (ch >= 'a' && ch <= 'f')
				digit = ch - 'a' + 10;
			else if (ch >= 'A' && ch <= 'F')
				digit = ch - 'A' + 10;
			else if (ch == '_')
				continue;
			else
				break;
			number = (number << 4) | digit;
			digits++;
		}
		_pos = i;
		if (digits > 16)
			return parserError("number is too big to fit 64 bits", _sharedIntegerToken);
		_sharedIntegerToken.setValue(number);
		return processIntegerSuffix();
	}
	
	protected Token processOctNumber() {
		_sharedIntegerToken.setPos(_startLine, _startPos);
		if (_pos >= _len)
			return parserError("Unexpected end of line in octal number", _sharedIntegerToken);
		int digits = 0;
		ulong number = 0;
		int i = _pos;
		bool overflow = false;
		for (;i < _len; i++) {
			dchar ch = _lineText[i];
			int digit = 0;
			if (ch >= '0' && ch <= '7')
				digit = ch - '0';
			else if (ch == '_')
				continue;
			else
				break;
			number <<= 3;
			if (digits >= 20) {
				if ((number >> 3) << 3 != number) {
					overflow = true;
					break;
				}
			}
			number |= digit;
			digits++;
		}
		_pos = i;
		if (overflow)
			return parserError("number is too big to fit 64 bits", _sharedIntegerToken);
		_sharedIntegerToken.setValue(number);
		return processIntegerSuffix();
	}
	
	// 
	protected Token processDecFloatSuffix(real value) {
        ubyte precision = 1;
        bool imaginary = false;
		dchar next = _pos < _len ? _lineText[_pos] : 0;
        if (next == 'f') {
            _pos++;
            precision = 0;
        } else if (next == 'L') {
            _pos++;
            precision = 2;
        }
		next = _pos < _len ? _lineText[_pos] : 0;
        if (next == 'i') {
            _pos++;
            imaginary = true;
        }
		next = _pos < _len ? _lineText[_pos] : 0;
        if (isIdentMiddleChar(next))
			return parserError("invalid suffix for floating point literal", _sharedRealToken);
		_sharedRealToken.setValue(value, precision, imaginary);
		return _sharedRealToken;
	}
	
	// after E char
	protected Token processDecFloatExponent(real value) {
		dchar next = _pos < _len ? _lineText[_pos] : 0;
		int sign = 1;
		if (next == '+') {
			_pos++;
		} else if (next == '-') {
			_pos++;
			sign = -1;
		}
		if (_pos >= _len)
			return parserError("Invalid exponent", _sharedRealToken);
		ulong digits = 0;
		ulong number = 0;
		int i = _pos;
		bool overflow = false;
		for (;i < _len; i++) {
			dchar ch = _lineText[i];
			uint digit = 0;
			if (ch >= '0' && ch <= '9')
				digit = ch - '0';
			else if (ch == '_')
				continue;
			else
				break;
			number *= 10;
			if (digits >= 18) {
				if ((number * 10) / 10 != number) {
					overflow = true;
					break;
				}
			}
			number += digit;
			digits++;
		}
		if (digits == 0)
			return parserError("Invalid exponent", _sharedRealToken);
		_pos = i;
		value *= pow(10., cast(long)number * sign);
		return processDecFloatSuffix(value);
	}
		
	protected Token processDecFloatSecondPart(ulong firstPart) {
		if (_pos >= _len) {
			_sharedRealToken.setValue(cast(real)firstPart);
			return _sharedRealToken;
		}
		ulong divider = 1;
		ulong number = 0;
		int i = _pos;
		bool overflow = false;
		for (;i < _len; i++) {
			dchar ch = _lineText[i];
			uint digit = 0;
			if (ch >= '0' && ch <= '9')
				digit = ch - '0';
			else if (ch == '_')
				continue;
			else
				break;
			if (divider * 10 < divider)
				continue; // ignore extra digits
			number *= 10;
			number += digit;
			divider *= 10;
		}
		_pos = i;
		real value = cast(real)firstPart + (cast(real)number / divider);
		dchar next = _pos < _len ? _lineText[_pos] : 0;
		if (next == 0) {
			// neither exponent nor suffix
			_sharedRealToken.setValue(value);
			return _sharedRealToken;
		}
   		if (next == 'e' || next == 'E') {
			_pos++;
			return processDecFloatExponent(value);
		}
		return processDecFloatSuffix(value);
	}
		
	protected Token processDecNumber(dchar c) {
		_sharedIntegerToken.setPos(_startLine, _startPos);
		_sharedRealToken.setPos(_startLine, _startPos);
		//if (_pos >= _len)
		//	return parserError("Unexpected end of line in number", _sharedIntegerToken);
		int digits = 1;
		ulong number = c - '0';
		int i = _pos;
		bool overflow = false;
		if (_line == _startLine) {
			for (;i < _len; i++) {
				dchar ch = _lineText[i];
				uint digit = 0;
				if (ch >= '0' && ch <= '9')
					digit = ch - '0';
				else if (ch == '_')
					continue;
				else
					break;
				number *= 10;
				if (digits >= 18) {
					if ((number * 10) / 10 != number) {
						overflow = true;
						break;
					}
				}
				number += digit;
				digits++;
			}
			_pos = i;
		}
		if (overflow)
			return parserError("number is too big to fit 64 bits", _sharedIntegerToken);
		_sharedIntegerToken.setValue(number);
		dchar next = _line == _startLine && _pos < _len ? _lineText[_pos] : 0;
		if (next == 0)
			return _sharedIntegerToken;
        if (next == 'e' || next == 'E') {
			_pos++;
            return processDecFloatExponent(number);
        } else if (next == '.') {
			_pos++;
			return processDecFloatSecondPart(number);
		}
		return processIntegerSuffix();
	}
		
    /// Either return InvalidToken or throw parser exception depending on current errorTolerant flag
	protected Token parserError(string msg, Token incompleteToken) {
        return parserError(msg, incompleteToken.line, incompleteToken.pos, incompleteToken.type);
    }
    /// Either return InvalidToken or throw parser exception depending on current errorTolerant flag
    protected Token parserError(string msg, int startLine, int startPos, TokenType failedTokenType = TokenType.INVALID) {
        if (_errorTolerant) {
            startPos--;
            _sharedInvalidToken.setPos(startLine, startPos);
            _sharedInvalidToken.errorMessage = msg;
            _sharedInvalidToken.errorCode = 1; // for future extension
            _sharedInvalidToken.invalidTokenType = failedTokenType; // for future extension
            // make invalid source text
            dchar[] invalidText;
            int p = startLine == _line ? startPos : 0;
            for (int i = p; i < _pos && i < _lineText.length; i++)
                invalidText ~= _lineText[i];

            // recover after error
            for (; _pos < _lineText.length; _pos++) {
                dchar ch = _lineText[_pos];
                if (ch == ' ' || ch == '\t' || ch == '(' || ch == ')' || ch == '[' || ch == ']' || ch == '{' || ch == '}')
                    break;
                if (failedTokenType == TokenType.INTEGER || failedTokenType == TokenType.FLOAT) {
                    if (ch == '*' || ch == '/')
                        break;
                }
                invalidText ~= ch;
            }
            _sharedInvalidToken.text = invalidText;
            return _sharedInvalidToken;
        }
		throw new ParserException(msg, _lineStream.file, _line, _pos);
	}

	protected Keyword detectKeyword(dchar ch) {
		if (ch > 'z')
			return Keyword.NONE;
		int len = _len - _pos;
		switch (cast(ubyte)ch) {
			//	ABSTRACT,
			//	ALIAS,
			//	ALIGN,
			//	ASM,
			//	ASSERT,
			//	AUTO,
			case 'a': return findKeyword(Keyword.ABSTRACT, Keyword.AUTO, _lineText.ptr + _pos, len, _pos);

			//	BODY,
			//	BOOL,
			//	BREAK,
			//	BYTE,
			case 'b': return findKeyword(Keyword.BODY, Keyword.BYTE, _lineText.ptr + _pos, len, _pos);
				
			//	CASE,
			//	CAST,
			//	CATCH,
			//	CDOUBLE,
			//	CENT,
			//	CFLOAT,
			//	CHAR,
			//	CLASS,
			//	CONST,
			//	CONTINUE,
			//	CREAL,
			case 'c': return findKeyword(Keyword.CASE, Keyword.CREAL, _lineText.ptr + _pos, len, _pos);
				
			//	DCHAR,
			//	DEBUG,
			//	DEFAULT,
			//	DELEGATE,
			//	DELETE,
			//	DEPRECATED,
			//	DO,
			//	DOUBLE,
			case 'd': return findKeyword(Keyword.DCHAR, Keyword.DOUBLE, _lineText.ptr + _pos, len, _pos);
				
			//	ELSE,
			//	ENUM,
			//	EXPORT,
			//	EXTERN,
			case 'e': return findKeyword(Keyword.ELSE, Keyword.EXTERN, _lineText.ptr + _pos, len, _pos);
				
			//	FALSE,
			//	FINAL,
			//	FINALLY,
			//	FLOAT,
			//	FOR,
			//	FOREACH,
			//	FOREACH_REVERSE,
			//	FUNCTION,
			case 'f': return findKeyword(Keyword.FALSE, Keyword.FUNCTION, _lineText.ptr + _pos, len, _pos);
				
			//	GOTO,
			case 'g': return findKeyword(Keyword.GOTO, Keyword.GOTO, _lineText.ptr + _pos, len, _pos);
				
			//	IDOUBLE,
			//	IF,
			//	IFLOAT,
			//	IMMUTABLE,
			//	IMPORT,
			//	IN,
			//	INOUT,
			//	INT,
			//	INTERFACE,
			//	INVARIANT,
			//	IREAL,
			//	IS,
			case 'i': return findKeyword(Keyword.IDOUBLE, Keyword.IS, _lineText.ptr + _pos, len, _pos);
				
			//	LAZY,
			//	LONG,
			case 'l': return findKeyword(Keyword.LAZY, Keyword.LONG, _lineText.ptr + _pos, len, _pos);
				
			//	MACRO,
			//	MIXIN,
			//	MODULE,
			case 'm': return findKeyword(Keyword.MACRO, Keyword.MODULE, _lineText.ptr + _pos, len, _pos);
				
			//	NEW,
			//	NOTHROW,
			//	NULL,
			case 'n': return findKeyword(Keyword.NEW, Keyword.NULL, _lineText.ptr + _pos, len, _pos);
				
			//	OUT,
			//	OVERRIDE,
			case 'o': return findKeyword(Keyword.OUT, Keyword.OVERRIDE, _lineText.ptr + _pos, len, _pos);
				
			//	PACKAGE,
			//	PRAGMA,
			//	PRIVATE,
			//	PROTECTED,
			//	PUBLIC,
			//	PURE,
			case 'p': return findKeyword(Keyword.PACKAGE, Keyword.PURE, _lineText.ptr + _pos, len, _pos);
				
			//	REAL,
			//	REF,
			//	RETURN,
			case 'r': return findKeyword(Keyword.REAL, Keyword.RETURN, _lineText.ptr + _pos, len, _pos);
				
			//	SCOPE,
			//	SHARED,
			//	SHORT,
			//	STATIC,
			//	STRUCT,
			//	SUPER,
			//	SWITCH,
			//	SYNCHRONIZED,
			case 's': return findKeyword(Keyword.SCOPE, Keyword.SYNCHRONIZED, _lineText.ptr + _pos, len, _pos);
				
			//	TEMPLATE,
			//	THIS,
			//	THROW,
			//	TRUE,
			//	TRY,
			//	TYPEDEF,
			//	TYPEID,
			//	TYPEOF,
			case 't': return findKeyword(Keyword.TEMPLATE, Keyword.TYPEOF, _lineText.ptr + _pos, len, _pos);
				
			//	UBYTE,
			//	UCENT,
			//	UINT,
			//	ULONG,
			//	UNION,
			//	UNITTEST,
			//	USHORT,
			case 'u': return findKeyword(Keyword.UBYTE, Keyword.USHORT, _lineText.ptr + _pos, len, _pos);
				
			//	VERSION,
			//	VOID,
			//	VOLATILE,
			case 'v': return findKeyword(Keyword.VERSION, Keyword.VOLATILE, _lineText.ptr + _pos, len, _pos);
				
			//	WCHAR,
			//	WHILE,
			//	WITH,
			case 'w': return findKeyword(Keyword.WCHAR, Keyword.WITH, _lineText.ptr + _pos, len, _pos);
				
			//	FILE,
			//	MODULE,
			//	LINE,
			//	FUNCTION,
			//	PRETTY_FUNCTION,
			//
			//	GSHARED,
			//	TRAITS,
			//	VECTOR,
			//	PARAMETERS,
			case '_': return findKeyword(Keyword.FILE, Keyword.PARAMETERS, _lineText.ptr + _pos, len, _pos);
			default: return Keyword.NONE;				
		}
	}	
	protected OpCode detectOp(dchar ch) nothrow {
		if (ch >= 128)
			return OpCode.NONE;
		dchar ch2 = _pos < _len ? _lineText[_pos] : 0;
		dchar ch3 = _pos < _len - 1 ? _lineText[_pos + 1] : 0;
		switch(cast(ubyte)ch) {
			//	DIV, 		//    /
			//	DIV_EQ, 	//    /=
			case '/':
				if (ch2 == '=') {
					_pos++;
					return OpCode.DIV_EQ;
				}
				return OpCode.DIV;
			//	DOT, 		//    .
			//	DOT_DOT, 	//    ..
			//	DOT_DOT_DOT,//    ...
			case '.':
				if (ch2 == '.') {
					if (ch3 == '.') {
						_pos += 2;
						return OpCode.DOT_DOT_DOT;
					}
					_pos++;
					return OpCode.DOT_DOT;
				}
				return OpCode.DOT;
			//	AND, 		//    &
			//	AND_EQ, 	//    &=
			//	LOG_AND, 	//    &&
			case '&':
				if (ch2 == '=') {
					_pos++;
					return OpCode.AND_EQ;
				}
				if (ch2 == '&') {
					_pos++;
					return OpCode.LOG_AND;
				}
				return OpCode.AND;
			//	OR, 		//    |
			//	OR_EQ, 		//    |=
			//	LOG_OR, 	//    ||
			case '|':
				if (ch2 == '=') {
					_pos++;
					return OpCode.OR_EQ;
				}
				if (ch2 == '|') {
					_pos++;
					return OpCode.LOG_OR;
				}
				return OpCode.OR;
			//	MINUS, 		//    -
			//	MINUS_EQ, 	//    -=
			//	MINUS_MINUS,//    --
			case '-':
				if (ch2 == '=') {
					_pos++;
					return OpCode.MINUS_EQ;
				}
				if (ch2 == '-') {
					_pos++;
					return OpCode.MINUS_MINUS;
				}
				return OpCode.MINUS;
			//	PLUS, 		//    +
			//	PLUS_EQ, 	//    +=
			//	PLUS_PLUS, 	//    ++
			case '+':
				if (ch2 == '=') {
					_pos++;
					return OpCode.PLUS_EQ;
				}
				if (ch2 == '+') {
					_pos++;
					return OpCode.PLUS_PLUS;
				}
				return OpCode.PLUS;
			//	LT, 		//    <
			//	LT_EQ, 		//    <=
			//	SHL, 		//    <<
			//	SHL_EQ, 	//    <<=
			//	LT_GT, 		//    <>
			//	NE_EQ, 		//    <>=
			case '<':
				if (ch2 == '<') {
					if (ch3 == '=') {
						_pos += 2;
						return OpCode.SHL_EQ;
					}
					_pos++;
					return OpCode.SHL;
				}
				if (ch2 == '>') {
					if (ch3 == '=') {
						_pos += 2;
						return OpCode.NE_EQ;
					}
					_pos++;
					return OpCode.LT_GT;
				}
				if (ch2 == '=') {
					_pos++;
					return OpCode.LT_EQ;
				}
				return OpCode.LT;
			//	GT, 		//    >
			//	GT_EQ, 		//    >=
			//	SHR_EQ		//    >>=
			//	ASR_EQ, 	//    >>>=
			//	SHR, 		//    >>
			//	ASR, 		//    >>>
			case '>':
				if (ch2 == '>') {
					if (ch3 == '>') {
						dchar ch4 = _pos < _len - 2 ? _lineText[_pos + 2] : 0;
						if (ch4 == '=') { // >>>=
							_pos += 3;
							return OpCode.ASR_EQ;
						}
						_pos += 2;
						return OpCode.ASR; // >>>
					}
					if (ch3 == '=') { // >>=
						_pos += 2;
						return OpCode.SHR_EQ;
					}
					_pos++;
					return OpCode.SHR;
				}
				if (ch2 == '=') { // >=
					_pos++;
					return OpCode.GT_EQ;
				}
				// >
				return OpCode.GT;
			//	NOT, 		//    !
			//	NOT_EQ		//    !=
			//	NOT_LT_GT, 	//    !<>
			//	NOT_LT_GT_EQ, //    !<>=
			//	NOT_LT, 	//    !<
			//	NOT_LT_EQ, 	//    !<=
			//	NOT_GT, 	//    !>
			//	NOT_GT_EQ, 	//    !>=
			case '!':
				if (ch2 == '<') { // !<
					if (ch3 == '>') { // !<>
						dchar ch4 = _pos < _len - 2 ? _lineText[_pos + 2] : 0;
						if (ch4 == '=') { // !<>=
							_pos += 3;
							return OpCode.NOT_LT_GT_EQ;
						}
						_pos += 2;
						return OpCode.NOT_LT_GT; // !<>
					}
					if (ch3 == '=') { // !<=
						_pos += 2;
						return OpCode.NOT_LT_EQ;
					}
					_pos++;
					return OpCode.NOT_LT; // !<
				}
				if (ch2 == '=') { // !=
					_pos++;
					return OpCode.NOT_EQ;
				}
				return OpCode.NOT;
			//	PAR_OPEN, 	//    (
			case '(':
				return OpCode.PAR_OPEN;
			//	PAR_CLOSE, 	//    )
			case ')':
				return OpCode.PAR_CLOSE;
			//	SQ_OPEN, 	//    [
			case '[':
				return OpCode.SQ_OPEN;
			//	SQ_CLOSE, 	//    ]
			case ']':
				return OpCode.SQ_CLOSE;
			//	CURL_OPEN, 	//    {
			case '{':
				return OpCode.CURL_OPEN;
			//	CURL_CLOSE, //    }
			case '}':
				return OpCode.CURL_CLOSE;
			//	QUEST, 		//    ?
			case '?':
				return OpCode.QUEST;
			//	COMMA, 		//    ,
			case ',':
				return OpCode.COMMA;
			//	SEMICOLON, 	//    ;
			case ';':
				return OpCode.SEMICOLON;
			//	COLON, 	    //    :
			case ':':
				return OpCode.COLON;
			//	DOLLAR, 	//    $
			case '$':
				return OpCode.DOLLAR;
			//	EQ, 		//    =
			//	QE_EQ, 		//    ==
			//	EQ_GT, 		//    =>
			case '=':
				if (ch2 == '=') { // ==
					_pos++;
					return OpCode.QE_EQ;
				}
				if (ch2 == '>') { // =>
					_pos++;
					return OpCode.EQ_GT;
				}
				return OpCode.EQ;
			//	MUL, 		//    *
			//	MUL_EQ, 	//    *=
			case '*':
				if (ch2 == '=') {
					_pos++;
					return OpCode.MUL_EQ;
				}
				return OpCode.MUL;
			//	MOD, 	//    %
			//	MOD_EQ, //    %=
			case '%':
				if (ch2 == '=') {
					_pos++;
					return OpCode.MOD_EQ;
				}
				return OpCode.MOD;
			//	XOR, 		//    ^
			//	XOR_EQ, 	//    ^=
			//	LOG_XOR, 	//    ^^
			//	LOG_XOR_EQ, //    ^^=
			case '^':
				if (ch2 == '^') {
					if (ch3 == '=') {
						_pos += 2;
						return OpCode.LOG_XOR_EQ;
					}
					_pos++;
					return OpCode.LOG_XOR;
				}
				if (ch2 == '=') {
					_pos++;
					return OpCode.XOR_EQ;
				}
				return OpCode.XOR;
			//	INV, 		//    ~
			//	INV_EQ, 	//    ~=
			case '~':
				if (ch2 == '=') {
					_pos++;
					return OpCode.INV_EQ;
				}
				return OpCode.INV;
			//	AT, 		//    @
			case '@':
				return OpCode.AT;
			//	SHARP 		//    #
			case '#':
				return OpCode.SHARP;
			default:
				return OpCode.NONE;
		}
	}
	
    protected Token processCharacterLiteral() {
		_sharedCharacterLiteralToken.setPos(_startLine, _startPos);
        if (_pos + 2 > _len)
            return parserError("Invalid character literal", _sharedCharacterLiteralToken);
        dchar ch = _lineText[_pos++];
        dchar ch2 = _lineText[_pos++];
        dchar type = 0;
        if (ch == '\\') {
            // process escaped character - store it in ch
            // TODO: support all escape sequences
            switch(ch2) {
                case 'r':
                    ch = '\r';
                    break;
                case 'n':
                    ch = '\n';
                    break;
                case 't':
                    ch = '\t';
                    break;
                case '\\':
                    ch = '\\';
                    break;
                default:
                    ch = ch2;
                    break;
            }
            // here must be closing '
            if (_pos + 1 > _len)
                return parserError("Invalid character literal", _sharedCharacterLiteralToken);
            ch2 = _lineText[_pos++];
        }
        if (ch2 != '\'')
            return parserError("Invalid character literal", _sharedCharacterLiteralToken);
        if (_pos < _len) {
            dchar t = _lineText[_pos];
            if (t == 'd' || t == 'w' || t == 'c') {
                type = t;
                _pos++;
            } else if (isIdentMiddleChar(ch)) {
                return parserError("Unexpected character after character literal", _sharedCharacterLiteralToken);
            }
        }
        _sharedCharacterLiteralToken.setCharacter(ch, type);
        return _sharedCharacterLiteralToken;
    }

	protected Token processDoubleQuotedOrWysiwygString(dchar delimiter) {
		bool wysiwyg = (delimiter == 'r' || delimiter == '`');
		//writeln("processDoubleQuotedString()");
		_sharedStringLiteralToken.setPos(_startLine, _startPos);
		_stringLiteralAppender.reset();
		if (delimiter == 'r') {
			_pos++;
			delimiter = '\"';
		}
		dchar type = 0;
		for (;;) {
			int i = _pos;
			int endPos = int.max;
            bool lastBackSlash = false;
			for(; i < _len; i++) {
                dchar ch = _lineText[i];
                if (ch == '\\') {
                    if (lastBackSlash)
                        lastBackSlash = false;
                    else
                        lastBackSlash = true;
                }
                else if (ch == delimiter && !lastBackSlash) {
					endPos = i;
					break;
				}
                else if(lastBackSlash)
                    lastBackSlash = false;
			}
			if (endPos != int.max) {
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
		dchar t = 0;
		if (_pos < _len) {
			dchar ch = _lineText[_pos];
			if (ch == 'c' || ch == 'w' || ch == 'd') {
				t = ch;
                _pos++;
                if (_pos < _len) {
                    ch = _lineText[_pos];
                    if (isIdentMiddleChar(ch))
                        return parserError("Unexpected character after string literal", _sharedStringLiteralToken);
                }
            } else if (isIdentMiddleChar(ch))
				return parserError("Unexpected character after string literal", _sharedStringLiteralToken);
		}
		if (t != 0) {
			if (type != 0 && t != type)
				return parserError("Cannot concatenate strings of different type", _sharedStringLiteralToken);
			type = t;
		}
		if (wysiwyg) {
			// no escape processing
			_sharedStringLiteralToken.setText(_stringLiteralAppender.get(), type);
			return _sharedStringLiteralToken;
		}
        _stringLiteralAppender.processEscapeSequences();
		_sharedStringLiteralToken.setText(_stringLiteralAppender.get(), type);
		return _sharedStringLiteralToken;
	}

	protected SysTime buildTime;
	
	//	string literal of the date of compilation "mmm dd yyyy"
	protected dstring formatBuildDate() {
		// TODO: provide proper format
		return to!dstring(buildTime);
	}
	
	//	string literal of the time of compilation "hh:mm:ss"
	protected dstring formatBuildTime() {
		// TODO: provide proper format
		return to!dstring(buildTime);
	}
	
	//	string literal of the date and time of compilation "www mmm dd hh:mm:ss yyyy"
	protected dstring formatBuildTimestamp() {
		// TODO: provide proper format
		return to!dstring(buildTime);
	}
	
	static immutable dstring VERSION = "0.1";
	static immutable dstring VENDOR = "coolreader.org";
	
	protected Token makeSpecialTokenString(dstring str, int pos) {
		_sharedStringLiteralToken.setPos(_startLine, _startPos);
		_sharedStringLiteralToken.setText(cast(dchar[])str, 0);
		return _sharedStringLiteralToken;
	}
	
	protected Token processSpecialToken(Keyword keyword, int pos) {
		switch (keyword) {
			//Special Token	Replaced with
			case Keyword.DATE: //	string literal of the date of compilation "mmm dd yyyy"
				return makeSpecialTokenString(formatBuildDate(), pos);
			case Keyword.TIME: //	string literal of the time of compilation "hh:mm:ss"
				return makeSpecialTokenString(formatBuildTime(), pos);
			case Keyword.TIMESTAMP: //	string literal of the date and time of compilation "www mmm dd hh:mm:ss yyyy"
				return makeSpecialTokenString(formatBuildTimestamp(), pos);
			case Keyword.VENDOR: //	Compiler vendor string, such as "Digital Mars D"
				return makeSpecialTokenString(VENDOR, pos);
			case Keyword.VERSION_: //	Compiler version as an integer, such as 2001
				return makeSpecialTokenString(VERSION, pos);
			default:
				parserError("Unknown special token", _line, pos);
		}
		return null;
	}
	
    protected int _startLine;
    protected int _startPos;

	// returns next token (clone it if you want to store for future usage, otherwise it may be overwritten by further nextToken() calls).
	Token nextToken() {
        _startLine = _line;
        _startPos = _pos;
		dchar ch = nextChar();
		if (ch == EOF_CHAR) {
			return emitEof();
		}
		if (ch == EOL_CHAR || ch == 0x0020 || ch == 0x0009 || ch == 0x000B || ch == 0x000C) {
			// white space (treat EOL as whitespace, too)
			return processWhiteSpace(ch);
		}
		dchar next = _pos < _len ? _lineText[_pos] : 0;
		if (ch == '/') {
			if (next == '/')
				return processOneLineComment();
			else if (next == '*')
				return processMultilineComment();
			else if (next == '+')
				return processNestedComment();
		}
        if (ch == '#' && _line == 1)
            return processOneLineSharpComment();
		if (ch == '\"')
			return processDoubleQuotedOrWysiwygString(ch);
		if (ch == '\'')
			return processCharacterLiteral();
		if (ch == 'x' && next == '\"')
			return processHexString();
		if (ch == 'q' && next == '\"')
			return processDelimitedString();
		if ((ch == 'r' && next == '\"') || (ch == '`'))
			return processDoubleQuotedOrWysiwygString(ch);
		int oldPos = _pos - 1;
		
		if (ch == '0') {
			if (next == 'b' || next == 'B')
				return processBinaryNumber();
			if (next == 'x' || next == 'X')
				return processHexNumber();
			if (next >= '0' && next <= '9')
				return processOctNumber();
			if (next >= '0' && next <= '9')
				return processDecNumber(ch);
		}
		if (ch >= '0' && ch <= '9')
			return processDecNumber(ch);
		if (ch == '.' && next >= '0' && next <= '9') // .123
			return processDecFloatSecondPart(0);
				
		if (ch == '_' || isUniversalAlpha(ch)) {
			// start of identifier or keyword?
			Keyword keyword = detectKeyword(ch);
			if (keyword != Keyword.NONE) {
				switch (keyword) {
					//Special Token	Replaced with
					case Keyword.EOF: return emitEof(); //	sets the scanner to the end of the file
					case Keyword.DATE: //	string literal of the date of compilation "mmm dd yyyy"
					case Keyword.TIME: //	string literal of the time of compilation "hh:mm:ss"
					case Keyword.TIMESTAMP: //	string literal of the date and time of compilation "www mmm dd hh:mm:ss yyyy"
					case Keyword.VENDOR: //	Compiler vendor string, such as "Digital Mars D"
					case Keyword.VERSION_: //	Compiler version as an integer, such as 2001
						return processSpecialToken(keyword, oldPos);
					default:
						_sharedKeywordToken.setPos(_startLine, _startPos);
						_sharedKeywordToken.keyword = keyword;
						return _sharedKeywordToken;
				}
			}
			return processIdent(ch);
		}
		OpCode op = detectOp(ch);
		if (op != OpCode.NONE) {
			_sharedOpToken.setPos(_startLine, _startPos);
			_sharedOpToken.opCode = op;
			return _sharedOpToken;
		}
        return parserError("Invalid token", _line, _pos);
	}

	
}

unittest {
    version(DisableLexerTest) {
    import std.stdio;
    import std.conv;
    import std.utf;
    import dlangui.core.linestream;
    string fname = "/home/lve/src/d/ddc/ddclexer/tests/tokenizer_test.d";
	writeln("opening file");
    try {
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
			    writeln("", token.line, ":", token.pos, "\t", token.toString);
	        }
        } catch (Exception e) {
            writeln("Exception " ~ e.toString);
        }
    } catch (Exception e) {
        writeln("Exception " ~ e.toString);
    }
    }
}

/// converts named entity to character, returns 0 if not found
dchar entityToChar(string name) {
    if (auto ch = name in entityToCharMap) {
        return *ch;
    }
    return 0;
}

/// fings entity name for character, returns null if not found
string charToEntity(dchar ch) {
    if (auto name = ch in charToEntityMap) {
        return *name;
    }
    return null;
}

private __gshared dchar[string]entityToCharMap;
private __gshared string[dchar]charToEntityMap;
private void addEntity(string name, dchar ch) {
    entityToCharMap[name] = ch;
    charToEntityMap[ch] = name;
}
__gshared static this() {
    addEntity("quot", 34);
    addEntity("amp",	38);
    addEntity("lt",	60);
    addEntity("gt",	62);
    addEntity("OElig",	338);
    addEntity("oelig",	339);
    addEntity("Scaron",	352);
    addEntity("scaron",	353);
    addEntity("Yuml",	376);
    addEntity("circ",	710);
    addEntity("tilde",	732);
    addEntity("ensp",	8194);
    addEntity("emsp",	8195);
    addEntity("thinsp",	8201);
    addEntity("zwnj",	8204);
    addEntity("zwj",	8205);
    addEntity("lrm",	8206);
    addEntity("rlm",	8207);
    addEntity("ndash",	8211);
    addEntity("mdash",	8212);
    addEntity("lsquo",	8216);
    addEntity("rsquo",	8217);
    addEntity("sbquo",	8218);
    addEntity("ldquo",	8220);
    addEntity("rdquo",	8221);
    addEntity("bdquo",	8222);
    addEntity("dagger",	8224);
    addEntity("Dagger",	8225);
    addEntity("permil",	8240);
    addEntity("lsaquo",	8249);
    addEntity("rsaquo",	8250);
    addEntity("euro",	8364);
    addEntity("nbsp",	160);
    addEntity("iexcl",	161);
    addEntity("cent",	162);
    addEntity("pound",	163);
    addEntity("curren",	164);
    addEntity("yen",	165);
    addEntity("brvbar",	166);
    addEntity("sect",	167);
    addEntity("uml",	168);
    addEntity("copy",	169);
    addEntity("ordf",	170);
    addEntity("laquo",	171);
    addEntity("not",	172);
    addEntity("shy",	173);
    addEntity("reg",	174);
    addEntity("macr",	175);
    addEntity("deg",	176);
    addEntity("plusmn",	177);
    addEntity("sup2",	178);
    addEntity("sup3",	179);
    addEntity("acute",	180);
    addEntity("micro",	181);
    addEntity("para",	182);
    addEntity("middot",	183);
    addEntity("cedil",	184);
    addEntity("sup1",	185);
    addEntity("ordm",	186);
    addEntity("raquo",	187);
    addEntity("frac14",	188);
    addEntity("frac12",	189);
    addEntity("frac34",	190);
    addEntity("iquest",	191);
    addEntity("Agrave",	192);
    addEntity("Aacute",	193);
    addEntity("Acirc",	194);
    addEntity("Atilde",	195);
    addEntity("Auml",	196);
    addEntity("Aring",	197);
    addEntity("AElig",	198);
    addEntity("Ccedil",	199);
    addEntity("Egrave",	200);
    addEntity("Eacute",	201);
    addEntity("Ecirc",	202);
    addEntity("Euml",	203);
    addEntity("Igrave",	204);
    addEntity("Iacute",	205);
    addEntity("Icirc",	206);
    addEntity("Iuml",	207);
    addEntity("ETH",	208);
    addEntity("Ntilde",	209);
    addEntity("Ograve",	210);
    addEntity("Oacute",	211);
    addEntity("Ocirc",	212);
    addEntity("Otilde",	213);
    addEntity("Ouml",	214);
    addEntity("times",	215);
    addEntity("Oslash",	216);
    addEntity("Ugrave",	217);
    addEntity("Uacute",	218);
    addEntity("Ucirc",	219);
    addEntity("Uuml",	220);
    addEntity("Yacute",	221);
    addEntity("THORN",	222);
    addEntity("szlig",	223);
    addEntity("agrave",	224);
    addEntity("aacute",	225);
    addEntity("acirc",	226);
    addEntity("atilde",	227);
    addEntity("auml",	228);
    addEntity("aring",	229);
    addEntity("aelig",	230);
    addEntity("ccedil",	231);
    addEntity("egrave",	232);
    addEntity("eacute",	233);
    addEntity("ecirc",	234);
    addEntity("euml",	235);
    addEntity("igrave",	236);
    addEntity("iacute",	237);
    addEntity("icirc",	238);
    addEntity("iuml",	239);
    addEntity("eth",	240);
    addEntity("ntilde",	241);
    addEntity("ograve",	242);
    addEntity("oacute",	243);
    addEntity("ocirc",	244);
    addEntity("otilde",	245);
    addEntity("ouml",	246);
    addEntity("divide",	247);
    addEntity("oslash",	248);
    addEntity("ugrave",	249);
    addEntity("uacute",	250);
    addEntity("ucirc",	251);
    addEntity("uuml",	252);
    addEntity("yacute",	253);
    addEntity("thorn",	254);
    addEntity("yuml",	255);
    addEntity("fnof",	402);
    addEntity("Alpha",	913);
    addEntity("Beta",	914);
    addEntity("Gamma",	915);
    addEntity("Delta",	916);
    addEntity("Epsilon",	917);
    addEntity("Zeta",	918);
    addEntity("Eta",	919);
    addEntity("Theta",	920);
    addEntity("Iota",	921);
    addEntity("Kappa",	922);
    addEntity("Lambda",	923);
    addEntity("Mu",	924);
    addEntity("Nu",	925);
    addEntity("Xi",	926);
    addEntity("Omicron",	927);
    addEntity("Pi",	928);
    addEntity("Rho",	929);
    addEntity("Sigma",	931);
    addEntity("Tau",	932);
    addEntity("Upsilon",	933);
    addEntity("Phi",	934);
    addEntity("Chi",	935);
    addEntity("Psi",	936);
    addEntity("Omega",	937);
    addEntity("alpha",	945);
    addEntity("beta",	946);
    addEntity("gamma",	947);
    addEntity("delta",	948);
    addEntity("epsilon",	949);
    addEntity("zeta",	950);
    addEntity("eta",	951);
    addEntity("theta",	952);
    addEntity("iota",	953);
    addEntity("kappa",	954);
    addEntity("lambda",	955);
    addEntity("mu",	956);
    addEntity("nu",	957);
    addEntity("xi",	958);
    addEntity("omicron",	959);
    addEntity("pi",	960);
    addEntity("rho",	961);
    addEntity("sigmaf",	962);
    addEntity("sigma",	963);
    addEntity("tau",	964);
    addEntity("upsilon",	965);
    addEntity("phi",	966);
    addEntity("chi",	967);
    addEntity("psi",	968);
    addEntity("omega",	969);
    addEntity("thetasym",	977);
    addEntity("upsih",	978);
    addEntity("piv",	982);
    addEntity("bull",	8226);
    addEntity("hellip",	8230);
    addEntity("prime",	8242);
    addEntity("Prime",	8243);
    addEntity("oline",	8254);
    addEntity("frasl",	8260);
    addEntity("weierp",	8472);
    addEntity("image",	8465);
    addEntity("real",	8476);
    addEntity("trade",	8482);
    addEntity("alefsym",	8501);
    addEntity("larr",	8592);
    addEntity("uarr",	8593);
    addEntity("rarr",	8594);
    addEntity("darr",	8595);
    addEntity("harr",	8596);
    addEntity("crarr",	8629);
    addEntity("lArr",	8656);
    addEntity("uArr",	8657);
    addEntity("rArr",	8658);
    addEntity("dArr",	8659);
    addEntity("hArr",	8660);
    addEntity("forall",	8704);
    addEntity("part",	8706);
    addEntity("exist",	8707);
    addEntity("empty",	8709);
    addEntity("nabla",	8711);
    addEntity("isin",	8712);
    addEntity("notin",	8713);
    addEntity("ni",	8715);
    addEntity("prod",	8719);
    addEntity("sum",	8721);
    addEntity("minus",	8722);
    addEntity("lowast",	8727);
    addEntity("radic",	8730);
    addEntity("prop",	8733);
    addEntity("infin",	8734);
    addEntity("ang",	8736);
    addEntity("and",	8743);
    addEntity("or",	8744);
    addEntity("cap",	8745);
    addEntity("cup",	8746);
    addEntity("int",	8747);
    addEntity("there4",	8756);
    addEntity("sim",	8764);
    addEntity("cong",	8773);
    addEntity("asymp",	8776);
    addEntity("ne",	8800);
    addEntity("equiv",	8801);
    addEntity("le",	8804);
    addEntity("ge",	8805);
    addEntity("sub",	8834);
    addEntity("sup",	8835);
    addEntity("nsub",	8836);
    addEntity("sube",	8838);
    addEntity("supe",	8839);
    addEntity("oplus",	8853);
    addEntity("otimes",	8855);
    addEntity("perp",	8869);
    addEntity("sdot",	8901);
    addEntity("lceil",	8968);
    addEntity("rceil",	8969);
    addEntity("lfloor",	8970);
    addEntity("rfloor",	8971);
    addEntity("loz",	9674);
    addEntity("spades",	9824);
    addEntity("clubs",	9827);
    addEntity("hearts",	9829);
    addEntity("diams",	9830);
    addEntity("lang",	10216);
    addEntity("rang",	10217);
}



//void runTokenizerTest()
unittest 
{
	import std.algorithm;
	class TokenTest {
		int _line;
		string _file;
		this(string file, int line) {
			_file = file;
			_line = line;
		}
		bool doTest(Token token) {
			return true;
		}		
		void execute(Tokenizer tokenizer) {
			Token token = tokenizer.nextToken();
			if (!doTest(token)) {
				assert(false, "	token doesn not match at " ~ _file ~ ":" ~ to!string(_line) ~ "  foundToken: " ~ token.toString ~ " expected: " ~ toString);
			}
		}
		public override @property string toString() {
			return "TokenTest";
		}
	}
	void testTokenizer(string code, TokenTest[] tokens, string file = __FILE__, uint line = __LINE__) {
		Tokenizer tokenizer = new Tokenizer(code, "tokenizerTest:" ~ file ~ ":" ~ to!string(line));
		for (int i = 0; i < tokens.length; i++) {
			tokens[i].execute(tokenizer);
		}
	}
	class KeywordTest : TokenTest {
		Keyword _code;
		this(Keyword code, string file = __FILE__, uint line = __LINE__) {
			super(file, line);
			_code = code;
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.KEYWORD)
				return false;
			if (token.keyword != _code)
				return false;
			return true;
		}		
		public override @property string toString() {
			return "Keyword:" ~ to!string(_code);
		}
	}
	class OpTest : TokenTest {
		OpCode _code;
		this(OpCode code, string file = __FILE__, uint line = __LINE__) {
			super(file, line);
			_code = code;
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.OP)
				return false;
			if (token.opCode != _code)
				return false;
			return true;
		}		
		public override @property string toString() {
			return "Op:" ~ to!string(_code);
		}
	}
	class StringTest : TokenTest {
		dstring _value;
        dchar _literalType;
		this(dstring value, dchar literalType = 0, string file = __FILE__, uint line = __LINE__) {
			super(file, line);
			_value = value;
            _literalType = literalType;
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.STRING)
				return false;
			if (!token.text.equal(_value))
				return false;
			if (token.literalType != _literalType)
				return false;
			return true;
		}		
		public override @property string toString() {
			return toUTF8("String:\"" ~ _value ~ "\"" ~ (_literalType ? _literalType : ' '));
		}
	}
	class IntegerTest : TokenTest {
		ulong _value;
		bool _unsigned;
		bool _long;
		this(ulong value, bool unsignedFlag = false, bool longFlag = false, string file = __FILE__, uint line = __LINE__) {
			super(file, line);
			_value = value;
			_unsigned = unsignedFlag;
			_long = longFlag;
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.INTEGER)
				return false;
			if (token.intValue != _value)
				return false;
			if (token.isUnsigned != _unsigned)
				return false;
			if (token.isLong != _long)
				return false;
			return true;
		}		
		public override @property string toString() {
			return "Integer:" ~ to!string(_value);
		}
	}
	class RealTest : TokenTest {
		real _value;
		ubyte _precision;
		bool _imaginary;
		this(real value, ubyte precision = 1, bool imaginary = false, string file = __FILE__, uint line = __LINE__) {
			super(file, line);
			_value = value;
			_precision = precision;
			_imaginary = imaginary;
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.FLOAT)
				return false;
            real diff = token.realValue - _value;
            real maxerr = _value / 1000000;
            if (diff < 0) diff = -diff;
            if (maxerr < 0) maxerr = -maxerr;
			if (diff > maxerr)
				return false;
			if (token.precision != _precision)
				return false;
			if (token.isImaginary != _imaginary)
				return false;
			return true;
		}		
		public override @property string toString() {
			return "Real:" ~ to!string(_value) ~ (_precision == 0 ? "f" : (_precision == 2 ? "L" : "")) ~ (_imaginary ? "i" : "");
		}
	}
	class IdentTest : TokenTest {
		string _value;
		this(string value, string file = __FILE__, uint line = __LINE__) {
			super(file, line);
			_value = value;
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.IDENTIFIER)
				return false;
			if (! to!string(token.text).equal(_value))
				return false;
			return true;
		}		
		public override @property string toString() {
			return "Ident:" ~ _value;
		}
	}
	class CommentTest : TokenTest {
		this(string file = __FILE__, uint line = __LINE__) {
			super(file, line);
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.COMMENT)
				return false;
			return true;
		}		
		public override @property string toString() {
			return "Comment";
		}
	}
	class EOFTest : TokenTest {
		this(string file = __FILE__, uint line = __LINE__) {
			super(file, line);
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.EOF)
				return false;
			return true;
		}		
		public override @property string toString() {
			return "EOF";
		}
	}
	class WhiteSpaceTest : TokenTest {
		this(string file = __FILE__, uint line = __LINE__) {
			super(file, line);
		}
		override bool doTest(Token token) {
			if (token.type != TokenType.WHITESPACE)
				return false;
			return true;
		}		
		public override @property string toString() {
			return "whiteSpace";
		}
	}
	TokenTest checkString(dstring value, dchar literalType = 0, string file = __FILE__, uint line = __LINE__) { 
		return new StringTest(value, literalType, file, line);
	}
	TokenTest checkInteger(ulong value, bool unsignedFlag = false, bool longFlag = false, string file = __FILE__, uint line = __LINE__) { 
		return new IntegerTest(value, unsignedFlag, longFlag, file, line);
	}
	TokenTest checkReal(real value, byte precision = 1, bool imaginary = false, string file = __FILE__, uint line = __LINE__) { 
		return new RealTest(value, precision, imaginary, file, line);
	}
	TokenTest checkIdent(string value, string file = __FILE__, uint line = __LINE__) { 
		return new IdentTest(value, file, line);
	}
	TokenTest checkKeyword(Keyword value, string file = __FILE__, uint line = __LINE__) { 
		return new KeywordTest(value, file, line);
	}
	TokenTest checkOp(OpCode value, string file = __FILE__, uint line = __LINE__) { 
		return new OpTest(value, file, line);
	}
	TokenTest checkSpace(string file = __FILE__, uint line = __LINE__) { 
		return new WhiteSpaceTest(file, line);
	}
	TokenTest checkComment(string file = __FILE__, uint line = __LINE__) { 
		return new CommentTest(file, line);
	}
	TokenTest checkEOF(string file = __FILE__, uint line = __LINE__) { 
		return new EOFTest(file, line);
	}

    // test strings
	testTokenizer("r\"simple\\nstring\"", [checkString( r"simple\nstring" )]);

    // test strings
	testTokenizer(q"TEST
"simple string"
"simple\nstring"
`simple string`
"simple string"d
"simple string"c
"simple string"w
"simple\&quot;string"
"\r\n\f\t\\\"\'&"
TEST"
                  , [
                      checkString("simple string"),
                      checkSpace(),
                      checkString("simple\nstring"),
                      checkSpace(),
                      checkString("simple string"),
                      checkSpace(),
                      checkString("simple string", 'd'),
                      checkSpace(),
                      checkString("simple string", 'c'),
                      checkSpace(),
                      checkString("simple string", 'w'),
                      checkSpace(),
                      checkString("simple\&quot;string"),
                      checkSpace(),
                      checkString("\r\n\f\t\\\"\'&"),
    ]);
    // basic test
	testTokenizer(q"TEST
int i;
TEST"
                  , [
                      checkKeyword(Keyword.INT),
                      checkSpace(),
                      checkIdent("i"),
                      checkOp(OpCode.SEMICOLON),
                      checkEOF()
                  ]);
    // test numbers
	testTokenizer("0b1101 0x123abcdU 0xABCL 0743 192837465 0 192_837_465 5.25 12.3f 54.1L 67.1i 3e3 25.67e-5f"
                  , [
                      checkInteger(13),
                      checkSpace(),
                      checkInteger(0x123abcd, true, false),
                      checkSpace(),
                      checkInteger(0xabc, false, true),
                      checkSpace(),
                      checkInteger(std.conv.octal!743),
                      checkSpace(),
                      checkInteger(192_837_465),
                      checkSpace(),
                      checkInteger(0),
                      checkSpace(),
                      checkInteger(192837465),
                      checkSpace(),
                      checkReal(5.25),
                      checkSpace(),
                      checkReal(12.3f, 0),
                      checkSpace(),
                      checkReal(54.1L, 2),
                      checkSpace(),
                      checkReal(67.1, 1, true),
                      checkSpace(),
                      checkReal(3e3),
                      checkSpace(),
                      checkReal(25.67e-5f, 0),
                      checkEOF()
                  ]);
}

