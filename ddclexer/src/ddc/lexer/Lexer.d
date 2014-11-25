// D grammar - according to http://dlang.org/grammar

module ddc.lexer.Lexer;
import ddc.lexer.LineStream;
import ddc.lexer.Tokenizer;

/** Lexem type constants */
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
    TEMPLATE_INSTANCE,
}

class Lexem {
	public @property LexemType type() { return LexemType.UNKNOWN; }
}

/** 
    Returns true for  one of keywords: bool, byte, ubyte, short, ushort, int, uint, long, ulong, 
        char, wchar, dchar, float, double, real, ifloat, idouble, ireal, cfloat, cdouble, creal, void 
*/
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

/** 
  Single token, one of keywords: bool, byte, ubyte, short, ushort, int, uint, long, ulong, 
  char, wchar, dchar, float, double, real, ifloat, idouble, ireal, cfloat, cdouble, creal, void
*/
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

/** 
    Returns true for  one of keywords: const, immutable, inout, shared 
*/
bool isTypeCtorToken(Token token) {
	if (token.type != TokenType.KEYWORD)
		return false;
	Keyword id = token.keyword;
	return id == Keyword.CONST
		|| id == Keyword.IMMUTABLE
		|| id == Keyword.INOUT
		|| id == Keyword.SHARED;
}

/** 
    Single token, one of keywords: const, immutable, inout, shared 
*/
class TypeCtor : Lexem {
	public Token _token;
	public override @property LexemType type() { return LexemType.TYPE_CTOR; }
	public this(Token token)
	in {
		assert(isTypeCtorToken(token));
	}
	body {
		_token = token;
	}
}

/** 
    Zero, one or several keywords: const, immutable, inout, shared 
*/
class TypeCtors : Lexem {
	public TypeCtor[] _list;
	public override @property LexemType type() { return LexemType.TYPE_CTORS; }
	public this(Token token)
	in {
		assert(isTypeCtorToken(token));
	}
	body {
		_list ~= new TypeCtor(token);
	}
	public void append(Token token)
	in {
		assert(isTypeCtorToken(token));
	}
	body {
		_list ~= new TypeCtor(token);
	}
}

/**
    Identifier list.

    IdentifierList:
        Identifier
        Identifier . IdentifierList
        TemplateInstance
        TemplateInstance . IdentifierList
 */
class IdentifierList : Lexem {
	public override @property LexemType type() { return LexemType.IDENTIFIER_LIST; }
	public this()
	in {
	}
	body {
	}
}

/**
    Template instance.

    TemplateInstance:
        Identifier TemplateArguments
*/
class TemplateInstance : Lexem {
	public override @property LexemType type() { return LexemType.TEMPLATE_INSTANCE; }
	public this()
	in {
	}
	body {
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
