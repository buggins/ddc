module main;

import std.stdio;
import std.conv;
import std.utf;
import ddc.lexer.LineStream;
import ddc.lexer.Tokenizer;

unittest {
    writeln("Hello from unittest");
}

void dumpLines(string code) {
    writeln("Dump lines for code \"", code, "\"\n");
	LineStream lines = LineStream.create(code, "code.d");
	for (;;) {
		dchar[] s = lines.readLine();
		if (s is null)
			break;
		writeln("line " ~ to!string(lines.line()) ~ ":" ~ toUTF8(s));
	}
}

void dumpTokens(string code) {
    writeln("Dump tokens:");
	LineStream lines = LineStream.create(code, "code.d");
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
        writeln("", token.line, ":", token.pos, "\t", token.toString, "\t`", token.text, "`");
    }
}

void main(string[] args)
{
    // Prints "Hello World" string in console
    string sampleCode = q"CODE
import std.stdio;
/* comment for function */
int f1(int x) {
return x * x * 2;
}
void main(string[] args) {
writeln("Hello world");

}
CODE";
	dumpLines(sampleCode);
	dumpTokens(sampleCode);
    // Lets the user press <Return> before program returns
    stdin.readln();
}

