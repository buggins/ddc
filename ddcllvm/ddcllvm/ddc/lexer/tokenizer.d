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
import std.string : startsWith;
import std.algorithm : equal;

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

/// token type, high 4 bits of 32 bit uint
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

/// token flags - bits 24..27 of uint
enum TokFlag : uint {
    none = 0x00000000,
    tokenString = 0x01000000,
}

enum CharType : uint {
    unknown = 0,
    char8 = 'c',
    char16 = 'w',
    char32 = 'd'
}

enum IntType : uint {
    int_default,
    int_unsigned,
    int_long,
    int_unsigned_long
}

enum FloatType : uint {
    float_default,
    float_short,
    float_long,
    float_default_im,
    float_short_im,
    float_long_im,
}

enum TokError : uint {
    none,
    InvalidToken,
    UnexpectedEofInComment,
    InvalidEscapeSequence,
    InvalidStringSuffix,
    InvalidStringDelimiter,
    InvalidHexString,
    InvalidCharacterLiteral,
    InvalidIntegerLiteral,
    InvalidHexLiteral,
    InvalidFloatLiteral,
    InvalidOctLiteral,
    InvalidFloatExponent,
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
    error_invalidStringDelimiter = TokType.error | TokError.InvalidStringDelimiter,
    error_invalidHexString = TokType.error | TokError.InvalidHexString,
    error_invalidCharacterLiteral = TokType.error | TokError.InvalidCharacterLiteral,
    error_invalidIntegerLiteral = TokType.error | TokError.InvalidIntegerLiteral,
    error_invalidHexLiteral = TokType.error | TokError.InvalidHexLiteral,
    error_invalidFloatLiteral = TokType.error | TokError.InvalidFloatLiteral,
    error_invalidOctLiteral = TokType.error | TokError.InvalidOctLiteral,
    error_invalidFloatExponent = TokType.error | TokError.InvalidFloatExponent,

    str_unknown = TokType.str,
    str_8 = TokType.str | CharType.char8,
    str_16 = TokType.str | CharType.char16,
    str_32 = TokType.str | CharType.char32,

    char_unknown = TokType.character,
    char_8 = TokType.character | CharType.char8,
    char_16 = TokType.character | CharType.char16,
    char_32 = TokType.character | CharType.char32,

    int_default = TokType.integer | IntType.int_default,
    int_unsigned = TokType.integer | IntType.int_unsigned,
    int_long = TokType.integer | IntType.int_long,
    int_unsigned_long = TokType.integer | IntType.int_unsigned_long,

    float_default = TokType.floating | FloatType.float_default,
    float_short = TokType.floating | FloatType.float_short,
    float_long = TokType.floating | FloatType.float_long,
    float_default_im = TokType.floating | FloatType.float_default_im,
    float_short_im = TokType.floating | FloatType.float_short_im,
    float_long_im = TokType.floating | FloatType.float_long_im,

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
	op_token_string_start = TokType.op | OpCode.TOKEN_STRING_START, //    q{ -- special op, for parsing token string content
	op_token_string_end = TokType.op | OpCode.TOKEN_STRING_END, //    } -- special op, for parsing token string content
	op_token_string_end_8 = TokType.op | OpCode.TOKEN_STRING_END_C, //    }c -- special op, for parsing token string content
	op_token_string_end_16 = TokType.op | OpCode.TOKEN_STRING_END_W, //    }w -- special op, for parsing token string content
	op_token_string_end_32 = TokType.op | OpCode.TOKEN_STRING_END_D, //    }d -- special op, for parsing token string content
	TOKEN_STRING_END, 		//    }  -- special op code, matching TOKEN_STRING_START
	TOKEN_STRING_END_C, 		//    }c  -- special op code, matching TOKEN_STRING_START
	TOKEN_STRING_END_W, 		//    }w  -- special op code, matching TOKEN_STRING_START
	TOKEN_STRING_END_D, 		//    }d  -- special op code, matching TOKEN_STRING_START
}

struct StrCache {
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
    /// token type (bits 28..31)
    @property TokType type() const { return cast(TokType)(id & 0xF0000000); }
    /// token code (bits 0..23)
    @property uint code() const { return (id & 0x00FFFFFF); }
    /// token flags bit set (bits 24..27)
    @property uint flags() const { return (id & 0x0F000000); }
    /// set token type
    @property void type(TokType t) {
        id = ((cast(uint)t) & 0xF0000000) | (id & 0x0FFFFFFF); 
    }
    /// set token flag
    @property void setFlag(TokFlag t) {
        id = t | (id & 0xF0FFFFFF); 
    }
    /// set token type and code
    void setType(TokType t, uint code) {
        id = ((cast(uint)t) & 0xF0000000) | (code & 0x00FFFFFF); 
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
            case TokType.integer:
            case TokType.floating:
            case TokType.str:
            case TokType.character:
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
            case TokType.integer:
            case TokType.floating:
            case TokType.str:
            case TokType.character:
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
    private StrCache * _identCache;
    private TextLines _source;
    private const(SourceLine) * _line;
    private int _lineIndex;
    private string _lineText;
    private int _lineLen;
    private int _pos;
    private Tok _token;

    private StringTokenMode _stringTokenMode = StringTokenMode.processed;
    private bool _wantsWhiteSpaces;

    /// string literal content processing mode
    @property StringTokenMode stringTokenMode() { return _stringTokenMode; }
    /// string literal content processing mode
    @property void stringTokenMode(StringTokenMode mode) { _stringTokenMode = mode; }

    /// do we need to return whitespaces
    @property bool wantsWhiteSpaces() { return _wantsWhiteSpaces; }
    /// do we need to return whitespaces
    @property void wantsWhiteSpaces(bool mode) { _wantsWhiteSpaces = mode; }



    void init(StrCache * identCache, TextLines source) {
        _identCache = identCache;
        _source = source;
        _lineIndex = -1;
        nextLine();
    }

    Tok[] getTokensForParser() {
        Tok[] res;
        int dst = 0;
        for(;;) {
            if (dst >= res.length) {
                res.length = res.length == 0 ? 8192 : res.length * 2;
            }
            res[dst++] = nextToken();
            if (_token.id == TokId.eof)
                return res[0..dst];
            if (_token.type == TokType.error) {
                writeln("error ", _token);
            }
        }
    }
    
    private void startToken() {
        _token.id = TokId.eof;
        _token.line = _line;
        _token.pos = _pos;
        _token.str = null;
    }

    private void updateTokenText() {
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
    dchar decodeChar() {
        if (_pos >= _lineLen)
            return 0;
        int nextPos; // unused
        return decodeChar(nextPos);
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
    			_pos += 1;
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
  				_pos += 1;
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
            if (_token.pos == 0 && _token.str.equal("Ddoc")) {
                // Ddoc up to end of file
                while(nextLine()) {
                    // move to next line
                }
                int startLine = _token.line.line;
                int startPos = _token.pos;
                _pos = _lineLen;
                _token.str = _source.rangeText(startLine, startPos, _line.line, _pos);
            }
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
        if (firstchar == 'r' || firstchar == 'x') {
            skipStart++;
            firstchar = s[skipStart];
        }
        if ((firstchar == '\"' && lastchar == '\"') || (firstchar == '`' && lastchar == '`') || (firstchar == '\'' && lastchar == '\'')) {
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
        if (res >= 0x110000) // too big for unicode character
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
		for (;;) {
			int i = _pos;
			int endPos = int.max;
            bool lastBackSlash = false;
			for (; i < _lineLen; i++) {
                char ch = _lineText[i];
                if (ch == '\\' && !wysiwyg) {
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
		int type = parseStringLiteralSuffix();
        if (type < 0)
            return;
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

    int parseStringLiteralSuffix() {
		int t = 0;
		if (_pos < _lineLen) {
			char ch = _lineText[_pos];
			if (ch == 'c' || ch == 'w' || ch == 'd') {
				t = ch;
                _pos++;
                if (_pos < _lineLen) {
                    dchar dch = decodeChar();
                    if (isIdentMiddleChar(dch)) {
                        updateTokenText();
                        _token.id = TokId.error_invalidStringSuffix;
                        return -1;
                    }
                }
            } else {
                dchar dch = decodeChar();
                if (isIdentMiddleChar(dch)) {
                    updateTokenText();
                    _token.id = TokId.error_invalidStringSuffix;
                    return -1;
                }
            }
		}
        return t;
    }

    void parseCharacterLiteral() {
        parseStringLiteral();
        if (_token.type == TokType.str) {
            switch(_token.id) {
                case TokId.str_8:
                    _token.id = TokId.char_8;
                    break;
                case TokId.str_16:
                    _token.id = TokId.char_16;
                    break;
                case TokId.str_32:
                    _token.id = TokId.char_32;
                    break;
                case TokId.str_unknown:
                default:
                    _token.id = TokId.char_unknown;
                    break;
            }
            string str = _token.str;
            if (_stringTokenMode == StringTokenMode.raw) {
                str = removeQuotes(str);
            }
            if (str.length != 1) {
                if (_stringTokenMode == StringTokenMode.rawnoquotes) {
                    if (!processEscapeSequences(_token.str)) {
                        _token.id = TokId.error_invalidEscapeSequence;
                        return;
                    }
                }
                if (str.length != 1) {
                    dstring dstr = toUTF32(str);
                    if (dstr.length != 1) {
                        // not a signle character
                        _token.id = TokId.error_invalidCharacterLiteral;
                    }
                }
            }
        }
    }

	void parseBinaryNumber() {
        // 0b
        _pos += 2;
        char ch;
        int digitCount = 0;
        while(_pos < _lineLen) {
            ch = _lineText[_pos];
            if (ch >= '0' && ch <= '1')
                digitCount++;
            else if (ch != '_')
                break;
            _pos++;
        }
        if (digitCount == 0) {
            _token.id = TokId.error_invalidIntegerLiteral;
            extendErrorWhileAlNum();
            return;
        }
        parseIntegerSuffix();
    }

	void parseHexNumber() {
        _pos += 2;
        char ch;
        int digitCount = 0;
        while(_pos < _lineLen) {
            ch = _lineText[_pos];
            int digit = parseHexDigit(ch);
            if (digit >= 0)
                digitCount++;
            else if (ch != '_')
                break;
            _pos++;
        }
        if (digitCount == 0) {
            _token.id = TokId.error_invalidIntegerLiteral;
            extendErrorWhileAlNum();
            return;
        }
        if (ch == 'p')
            parseDecFloatExponent();
        else if (ch == '.') {
            char ch2 = (_pos + 1 < _lineLen) ? _lineText[_pos + 1] : 0;
            if (parseHexDigit(ch2) >= 0)
                parseHexFloatSecondPart();
            else
                parseIntegerSuffix();
        } else
            parseIntegerSuffix();
    }
	void parseOctNumber() {
        // 0[0..7]
        char ch;
        int digitCount = 0;
        while(_pos < _lineLen) {
            ch = _lineText[_pos];
            if (ch >= '0' && ch <= '7')
                digitCount++;
            else if (ch != '_')
                break;
            _pos++;
        }
        if (digitCount == 0) {
            _token.id = TokId.error_invalidIntegerLiteral;
            extendErrorWhileAlNum();
            return;
        }
        parseIntegerSuffix();
    }
	void parseDecNumber() {
        // [0..9]
        char ch;
        int digitCount = 0;

        while(_pos < _lineLen) {
            ch = _lineText[_pos];
            if (ch >= '0' && ch <= '9')
                digitCount++;
            else if (ch != '_')
                break;
            _pos++;
        }
        if (digitCount == 0) {
            _token.id = TokId.error_invalidIntegerLiteral;
            extendErrorWhileAlNum();
            return;
        }
        ch = (_pos < _lineLen) ? _lineText[_pos] : 0;
        char ch2 = (_pos + 1 < _lineLen) ? _lineText[_pos + 1] : 0;
        if (ch == 'e' || ch == 'E') {
            parseDecFloatExponent();
            return;
        }
        if (ch == '.') {
            if (ch2 >= '0' && ch2 <= '9') {
                parseDecFloatSecondPart();
                return;
            }
        }
        if (ch == 'l' || ch == 'L' || ch == 'u' || ch == 'U') {
            parseIntegerSuffix();
            return;
        }
        if (ch == 'f' || ch == 'F' || ch == 'L' || ch == 'i' || ch == 'I') {
            parseDecFloatSuffix();
            return;
        }
        updateTokenText();
        dchar dch = decodeChar();
        if (isIdentMiddleChar(dch)) {
            _token.id = TokId.error_invalidIntegerLiteral;
            extendErrorWhileAlNum();
            return;
        }
        // TODO: validate
        _token.id = TokId.int_default;
    }
    void extendErrorWhileAlNum() {
        while (_pos < _lineLen) {
            int nextPos;
            dchar dch = decodeChar(nextPos);
            if (!isIdentMiddleChar(dch))
                break;
            _pos = nextPos;
        }
        updateTokenText();
    }
    void parseIntegerSuffix() {
        // current char is u U l L
        bool u_suffix = false;
        bool l_suffix = false;
        char ch = _pos < _lineLen ? _lineText[_pos] : 0;
        if (ch == 'u' || ch == 'U') {
            u_suffix = true;
            _pos++;
            ch = _pos < _lineLen ? _lineText[_pos] : 0;
        }
        if (ch == 'l' || ch == 'L') {
            l_suffix = true;
            _pos++;
        }
        dchar dch = decodeChar();
        if (isIdentMiddleChar(dch)) {
            _token.id = TokId.error_invalidIntegerLiteral;
            extendErrorWhileAlNum();
            return;
        }
        updateTokenText();
        if (u_suffix) {
            if (l_suffix)
                _token.id = TokId.int_unsigned_long;
            else
                _token.id = TokId.int_unsigned;
        } else {
            if (l_suffix)
                _token.id = TokId.int_long;
            else
                _token.id = TokId.int_default;
        }
    }
    void parseDecFloatSuffix() {
        // current char is u U l L f F
        bool i_suffix = false;
        bool l_suffix = false;
        bool f_suffix = false;
        char ch = _pos < _lineLen ? _lineText[_pos] : 0;
        if (ch == 'l' || ch == 'L') {
            l_suffix = true;
            _pos++;
            ch = _pos < _lineLen ? _lineText[_pos] : 0;
        }
        if (ch == 'f' || ch == 'F') {
            f_suffix = true;
            _pos++;
            ch = _pos < _lineLen ? _lineText[_pos] : 0;
        }
        if (ch == 'i' || ch == 'I') {
            i_suffix = true;
            _pos++;
        }
        dchar dch = decodeChar();
        if (isIdentMiddleChar(dch) || (l_suffix && f_suffix)) {
            _token.id = TokId.error_invalidFloatLiteral;
            extendErrorWhileAlNum();
            return;
        }
        updateTokenText();
        if (f_suffix) {
            if (i_suffix)
                _token.id = TokId.float_short_im;
            else
                _token.id = TokId.float_short;
        } else if (l_suffix) {
            if (i_suffix)
                _token.id = TokId.float_long_im;
            else
                _token.id = TokId.float_long;
        } else {
            if (i_suffix)
                _token.id = TokId.float_default_im;
            else
                _token.id = TokId.float_default;
        }
    }
    void parseDecFloatExponent() {
        // current char is e or E (or p for hex float literal)
        _pos++;
        char ch = _pos < _lineLen ? _lineText[_pos] : 0;
        if (ch != '+' && ch != '-' && (ch < '0' || ch > '9')) {
            _token.id = TokId.error_invalidFloatLiteral;
            extendErrorWhileAlNum();
            return;
        }
        _pos++;
        int digitCount = (ch >= '0' && ch <= '9') ? 1 : 0;
        while(_pos < _lineLen) {
            ch = _lineText[_pos];
            if (ch >= '0' && ch <= '9')
                digitCount++;
            else if (ch != '_')
                break;
            _pos++;
        }
        if (digitCount == 0) {
            _token.id = TokId.error_invalidFloatExponent;
            extendErrorWhileAlNum();
            return;
        }
        parseDecFloatSuffix();
    }
	void parseHexFloatSecondPart() {
        // current char is .
        _pos++;
        char ch;
        int digitCount = 0;
        while(_pos < _lineLen) {
            ch = _lineText[_pos];
            if (parseHexDigit(ch) >= 0)
                digitCount++;
            else if (ch != '_')
                break;
            _pos++;
        }
        if (ch == 'p') {
            parseDecFloatExponent();
            return;
        }
        parseDecFloatSuffix();
    }
	void parseDecFloatSecondPart() {
        // current char is .
        _pos++;
        char ch;
        int digitCount = 0;
        while(_pos < _lineLen) {
            ch = _lineText[_pos];
            if (ch >= '0' && ch <= '9')
                digitCount++;
            else if (ch != '_')
                break;
            _pos++;
        }
        if (ch == 'e' || ch == 'E') {
            parseDecFloatExponent();
            return;
        }
        parseDecFloatSuffix();
    }

    void parseHexString() {
        // x"
        _pos += 2;

		for (;;) {
			int i = _pos;
			int endPos = int.max;
            bool lastBackSlash = false;
			for (; i < _lineLen; i++) {
                char ch = _lineText[i];
                if (ch == '\"') {
					endPos = i;
					break;
				}
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
                    dchar dch = decodeChar();
                    if (isIdentMiddleChar(dch)) {
                        updateTokenText();
                        _token.id = TokId.error_invalidStringSuffix;
                        return;
                    }
                }
            } else {
                dchar dch = decodeChar();
                if (isIdentMiddleChar(dch)) {
                    updateTokenText();
                    _token.id = TokId.error_invalidStringSuffix;
                    return;
                }
            }
		}
        updateTokenText();
        _token.setType(TokType.str, t);
        if (_stringTokenMode == StringTokenMode.raw) {
            return;
        }

        _token.str = removeQuotes(_token.str);

		if (_stringTokenMode == StringTokenMode.rawnoquotes) {
			// no escape processing
			return;
		}
        
        string str = _token.str;
        char[] buf;
        int digitIndex = 0;
        int n = 0;
        for (int i = 0; i < str.length; i++) {
            char ch = str[i];
            if (ch == 0x0020 || ch == 0x0009 || ch == 0x000B || ch == 0x000C)
                continue; // skip whitespace
            int digit = parseHexDigit(ch);
            if (digit < 0) {
                _token.id = TokId.error_invalidHexString;
                return;
            }
            n = (n << 4) | digit;
            digitIndex++;
            if (digitIndex == 2) {
                digitIndex = 0;
                buf ~= cast(char)(n & 0xFF);
            }
        }
        if (digitIndex == 1) {
            _token.id = TokId.error_invalidHexString;
            return;
        }
        _token.str = buf.dup;
    }

    void parseDelimitedString() {
        // q"
        _pos += 2;
        if (_pos >= _lineLen) {
            _token.id = TokId.error_invalidStringDelimiter;
            return;
        }

        char delimChar = _lineText[_pos];
        char matchingChar = delimChar;

        int nextPos = -1;
        dchar dch = decodeChar(nextPos);

        int contentStartLine = _line.line;
        int contentStartPos = _pos + 1;
        int contentEndLine = -1;
        int contentEndPos = -1;

        if (isIdentStartChar(dch)) {
            string delimIdent;
            int identStart = _pos;
            // delimiter identifier
            _pos = nextPos;
            while (_pos < _lineLen) {
                dch = decodeChar(nextPos);
                if (!isIdentMiddleChar(dch)) {
                    _token.id = TokId.error_invalidStringDelimiter;
                    return;
                }
                _pos = nextPos;
            }
            // end of line after delimiter
            delimIdent = _lineText[identStart .. _pos];
            // go to next line (newline char is not included into string)
            if (!nextLine()) {
                updateTokenText();
                _token.id = TokId.error_invalidStringDelimiter;
                return;
            }
            delimChar = 0; // use delimIdent
            contentStartLine = _line.line;
            contentStartPos = _pos;
            for(;;) {
                // TODO: find matching ident
                if (_lineText.startsWith(delimIdent)) {
                    if (_lineLen > delimIdent.length && _lineText[delimIdent.length] == '\"') {
                        contentEndLine = _line.line;
                        contentEndPos = _pos;
                        _pos = cast(int)(delimIdent.length) + 1;
                        break;
                    }
                }
                if (!nextLine())
                    break;
            }
        } else {
            switch (delimChar) {
                case '[':
                    matchingChar = ']';
                    break;
                case '(':
                    matchingChar = ')';
                    break;
                case '<':
                    matchingChar = '>';
                    break;
                case '{':
                    matchingChar = '}';
                    break;
                default:
                    matchingChar = delimChar;
                    break;
            }
            _pos++;
		    for (;;) {
			    int i = _pos;
			    for (; i + 1 < _lineLen; i++) {
                    char ch = _lineText[i];
                    char ch2 = _lineText[i + 1];
                    if (ch == matchingChar && ch2 == '\"') {
                        contentEndLine = _line.line;
                        contentEndPos = i;
                        _pos = i + 2;
					    break;
				    }
			    }
			    if (contentEndPos > 0) {
				    // found end quote
				    break;
			    }
			    // no quote by end of line
			    if (!nextLine()) {
				    // do we need to throw exception if eof comes before end of string?
				    break;
			    }
		    }
        }
        // now _pos is end of literal, and content Start/End line / pos is content
        if (contentEndLine < 0) {
            updateTokenText();
            // not found
            _token.id = TokId.error_invalidStringDelimiter;
            return;
        }

		int type = parseStringLiteralSuffix();
        if (type < 0)
            return; // invalid string literal suffix
        _token.setType(TokType.str, type);

        if (_stringTokenMode == StringTokenMode.raw) {
            // include delimiters
            updateTokenText();
            return;
        }

        // text w/o delimiters
        _token.str = _source.rangeText(contentStartLine, contentStartPos, contentEndLine, contentEndPos);
    }

    void parseTokenString() {
        // q{
        _pos += 2;
        updateTokenText();
        _token.id = TokId.op_token_string_start;
        handleTokenStringToken();
    }

    void parseCurlyOpen() {
        // just curly open
        _pos++;
        updateTokenText();
        _token.id = TokId.op_curl_open;
        if (_tokenStringStack.length > 0)
            handleTokenStringToken(); // put on stack if inside token string
    }

    void parseCurlyClose() {
        if (wantTokenStringEnd()) {
            // it's token string end
            _pos++;
            int type = TokId.op_token_string_end;
            char ch = _pos < _lineLen ? _lineText[_pos] : 0;
            if (ch == 'c' || ch == 'w' || ch == 'd') {
                _pos++;
                if (!isIdentMiddleChar(decodeChar())) {
                    if (ch == 'c')
                        type = TokId.op_token_string_end_8;
                    else if (ch == 'w')
                        type = TokId.op_token_string_end_16;
                    else
                        type = TokId.op_token_string_end_32;
                } else {
                    // back
                    _pos--;
                }
            }
            updateTokenText();
            _token.id = type;
            handleTokenStringToken();
        } else {
            // just curly close op
            _pos++;
            updateTokenText();
            _token.id = TokId.op_curl_close;
            if (_tokenStringStack.length > 0)
                handleTokenStringToken();
        }
    }

    private Tok[] _tokenStringStack;
    /// returns true if } should be considered as token string end
    private bool wantTokenStringEnd() {
        return _tokenStringStack.length > 0 && _tokenStringStack[$ - 1].id == TokId.op_token_string_start;
    }

    void handleTokenStringToken() {
        if (_token.id == TokId.op_token_string_start || _token.id == TokId.op_curl_open) {
            // put { or q{ on stack
            _tokenStringStack ~= _token;
        } else if (_token.id == TokId.op_curl_close) {
            if (_tokenStringStack.length > 0) {
                _tokenStringStack.length--;
            }
        } else if (_token.id == TokId.op_token_string_end || _token.id == TokId.op_token_string_end_8 || _token.id == TokId.op_token_string_end_16 || _token.id == TokId.op_token_string_end_32) {
            if (_tokenStringStack.length > 0) {
                _tokenStringStack.length--;
            }
        }
    }

    /// parse next token (including token string and string literal concatenation support)
    Tok nextToken() {
        for(;;) {
            nextTokenRaw();
            while (_tokenStringStack.length > 0) {
                // possible token string end
                Tok startToken = _tokenStringStack[0];
                nextTokenRaw();
                if (_tokenStringStack.length == 0 || _token.id == TokId.eof) {
                    // closed string token
                    int type = TokId.str_unknown;
                    if (_token.id == TokId.op_token_string_end_8)
                        type = TokId.str_8;
                    else if (_token.id == TokId.op_token_string_end_16)
                        type = TokId.str_16;
                    else if (_token.id == TokId.op_token_string_end_32)
                        type = TokId.str_32;
                    if (_stringTokenMode == StringTokenMode.raw) {
                        // including q{ and }
                        _token.str = _source.rangeText(startToken.line.line, startToken.pos, _token.line.line, _token.pos + cast(int)_token.str.length);
                        _token.line = startToken.line;
                        _token.pos = startToken.pos;
                    } else {
                        // w/o q{ and }
                        _token.str = _source.rangeText(startToken.line.line, startToken.pos + 2, _token.line.line, _token.pos);
                        _token.line = startToken.line;
                        _token.pos = startToken.pos;
                    }
                    _token.id = type;
                    break;
                }
            }
            if (_wantsWhiteSpaces || _token.type != TokType.whitespace)
                break;
        }
        return _token;
    }

    Tok nextTokenRaw() {
        nextTokenRawInternal();
        return _token;
    }

    /// parse next token, no string literals join, return internal token string tokens as tokens
    void nextTokenRawInternal() {
        startToken();
        if (_line.line == 0) {
            if (_lineText.startsWith("#!")) {
                // remove script line
                nextLine();
                updateTokenText();
                _token.id = TokId.comment_single; // single line comment
                return;
            }
        }
        if (_pos >= _lineLen) {
            // whitespace or eof
            parseWhitespace();
            return;
        }
        char ch = _lineText[_pos];
        if (ch == 0x0020 || ch == 0x0009 || ch == 0x000B || ch == 0x000C) {
            parseWhitespace();
            return;
        }
        char ch2 = _pos + 1 < _lineLen ? _lineText[_pos + 1] : 0;
        char ch3 = _pos + 2 < _lineLen ? _lineText[_pos + 2] : 0;
        if (ch == '/' && ch2 == '/') {
            parseSingleLineComment();
            return;
        }
        if (ch == '/' && ch2 == '*') {
            parseMultilineComment();
            return;
        }
        if (ch == '/' && ch2 == '+') {
            parseNestedComment();
            return;
        }
        if (ch == 'x' && ch2 == '\"') {
            parseHexString();
            return;
        }
        if (ch == 'q' && ch2 == '\"') {
            parseDelimitedString();
            return;
        }
        if (ch == 'q' && ch2 == '{') {
            parseTokenString();
            return;
        }
        if (ch == '\"') {
            parseStringLiteral();
            return;
        }
        if (ch == '`') {
            parseStringLiteral();
            return;
        }
        if (ch == '\'') {
            parseCharacterLiteral();
            return;
        }
        if (ch == 'r' && ch2 == '\"') {
            parseStringLiteral();
            return;
        }
        if (ch == '}') {
            // special processing, for support of token strings
            parseCurlyClose();
            return;
        }
        if (ch == '{') {
            // special processing, for support of token strings
            parseCurlyOpen();
            return;
        }
		if (ch == '0') {
			if (ch2 == 'b' || ch2 == 'B') {
				parseBinaryNumber();
                return;
            }
			if (ch2 == 'x' || ch2 == 'X') {
				parseHexNumber();
                return;
            }
			if (ch2 >= '0' && ch2 <= '7') {
				parseOctNumber();
                return;
            }
			if (ch2 >= '0' && ch2 <= '9') {
				parseDecNumber();
                return;
            }
		}
		if (ch >= '0' && ch <= '9') {
			parseDecNumber();
            return;
        }
		if (ch == '.' && ch2 >= '0' && ch2 <= '9') { // .123
			parseDecFloatSecondPart();
            return;
        }

        OpCode op = detectOp();
        if (op != OpCode.NONE) {
            _token.setType(TokType.op, op);
            updateTokenText();
            return;
        }
        int nextPos;
        dchar dch = decodeChar(nextPos);
        if (isIdentStartChar(dch)) {
            _pos = nextPos;
            parseIdentOrKeyword();
            return;
        }
        errorToken();
        return;
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
	SHARP, 		//    #
	TOKEN_STRING_START, 		//    q{  -- special op
	TOKEN_STRING_END, 		//    }  -- special op code, matching TOKEN_STRING_START
	TOKEN_STRING_END_C, 		//    }c  -- special op code, matching TOKEN_STRING_START
	TOKEN_STRING_END_W, 		//    }w  -- special op code, matching TOKEN_STRING_START
	TOKEN_STRING_END_D, 		//    }d  -- special op code, matching TOKEN_STRING_START
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
	"#",
	"q{",
	"}", // end of token string, no type postfix (special token code)
	"}c",// end of token string, char (special token code)
	"}w",// end of token string, wchar (special token code)
	"}d",// end of token string, dchar (special token code)
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


/// Returns timestamp in milliseconds since 1970 UTC similar to Java System.currentTimeMillis()
@property long currentTimeMillis() {
    return std.datetime.Clock.currStdTime / 10000;
}
