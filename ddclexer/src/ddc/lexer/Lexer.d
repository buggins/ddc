module ddc.lexer.Lexer;
import ddc.lexer.LineStream;

class Lexer
{
	LineStream _lineStream;
    this(LineStream lineStream)
    {
        _lineStream = lineStream;
    }
}
