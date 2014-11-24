module ddc.lexer.Lexer;
import ddc.lexer.LineStream;
import ddc.lexer.Tokenizer;

enum LexemType : ushort {
	UNKNOWN,
	// types
	TYPE,
	TYPE_CTORS,
	TYPE_CTOR,
	BASIC_TYPE,
	BASIC_TYPE_X,
	BASIC_TYPE_2,
	IDENTIFIER_LIST,
	TYPEOF,
}

class Lexem {
	public @property LexemType type() { return LexemType.UNKNOWN; }
}

// Returns true for  one of keywords: bool, byte, ubyte, short, ushort, int, uint, long, ulong, 
//     char, wchar, dchar, float, double, real, ifloat, idouble, ireal, cfloat, cdouble, creal, void
bool isBasicTypeXToken(Token token) {
	if (token.type != TokenType.KEYWORD)
		return false;
	Keyword id = token.keyword;
	return id == Keyword.BOOL
		|| id == Keyword.BYTE
		|| id == Keyword.UBYTE
		|| id == Keyword.SHORT
		|| id == Keyword.USHORT
		|| id == Keyword.INT
		|| id == Keyword.UINT
		|| id == Keyword.LONG
		|| id == Keyword.ULONG
		|| id == Keyword.CHAR
		|| id == Keyword.WCHAR
		|| id == Keyword.DCHAR
		|| id == Keyword.FLOAT
		|| id == Keyword.DOUBLE
		|| id == Keyword.REAL
		|| id == Keyword.IFLOAT
		|| id == Keyword.IDOUBLE
		|| id == Keyword.IREAL
		|| id == Keyword.CFLOAT
		|| id == Keyword.CDOUBLE
		|| id == Keyword.CREAL
		|| id == Keyword.VOID;
}

// Single token, one of bool, byte, ubyte, short, ushort, int, uint, long, ulong, 
// char, wchar, dchar, float, double, real, ifloat, idouble, ireal, cfloat, cdouble, creal, void
class BasicTypeX : Lexem {
	public Token _token;
	public override @property LexemType type() { return LexemType.BASIC_TYPE_X; }
	public this(Token token) 
	in {
		assert(isBasicTypeXToken(token));
	}
	body {
		_token = token;
	}
}

class Lexer
{
	LineStream _lineStream;
    this(LineStream lineStream)
    {
        _lineStream = lineStream;
    }
}
