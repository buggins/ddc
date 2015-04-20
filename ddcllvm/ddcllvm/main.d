module fibonacci;

import std.conv : to;
import std.stdio : writefln, writeln, readln;

import llvm.c;

import llvm.util.memory;

import llvmtest;
import lexertest;
import ddc.lexer.linestream;
import ddc.lexer.tokenizer;

int main(string[] args)
{
    version(Windows) { //TokenizerTest
    testTokenizer(q{//some comment
        /******
         some text for multiline comment
        **********/
        /+  /* bla bla */  /+ some nested comment +/ hjk
        ++++++++++++++++++/
        for (someIdent123=) >>>= 
            "Some\x35\u0036\U00000037\&quot;. \n text" `Bla \n bla`d r"bla bla"w "some\t\ttext тест"c
            'a'
            'b'c
            'c'w
            'd'd
            `\/`
            125
            12_785u
            125L
            1_25UL
            0x12_34567,0b01010111
            0x1234567U,0b010_10111u
            0x1234567L,0b0101_0111L
            0x1234567uL,0b01_010111UL
            0.1,.5,343_432e10,3.5e-3,432_6.5e+5
            0.1f,.5f,343_432e10f,3.5e-3f,432_6.5e+5f
            0.1L,.5L,343_432e10L,3.5e-3L,432_6.5e+5L
            0.1i,.5i,343_432e10i,3.5e-3i,432_6.5e+5i
            0.1fi,.5fi,343_432e10fi,3.5e-3fi,432_6.5e+5fi
            0.1Li,.5Li,343_432e10Li,3.5e-3Li,432_6.5e+5Li
            0x1_FFFFp3,0x1Ap-3L,0x2Bp+1f,0x234.543Fp0L
            q{TOKEN() STRING{}} q{one.
                    more[token]
                    *
                    string}w
            x"f0 2e 5624"
            q"{bla bla [1] ex}"d q"/some text/"w q"DOC
line 1
line 2
DOC"c
    }, [
        TokId.comment_single, TokId.whitespace, 
        TokId.comment_multi, TokId.whitespace, 
        TokId.comment_nested, TokId.whitespace, 
        TokId.kw_for, TokId.whitespace, TokId.op_par_open, TokId.ident, TokId.op_eq,
        TokId.op_par_close, TokId.whitespace, TokId.op_asr_eq, TokId.whitespace,
        TokId.str_unknown, TokId.whitespace, 
        TokId.str_32, TokId.whitespace, 
        TokId.str_16, TokId.whitespace, 
        TokId.str_8, TokId.whitespace, 
        TokId.char_unknown, TokId.whitespace, 
        TokId.char_8, TokId.whitespace, 
        TokId.char_16, TokId.whitespace, 
        TokId.char_32, TokId.whitespace, 
        TokId.str_unknown, TokId.whitespace, 
        TokId.int_default, TokId.whitespace, 
        TokId.int_unsigned, TokId.whitespace, 
        TokId.int_long, TokId.whitespace, 
        TokId.int_unsigned_long, TokId.whitespace, 
        TokId.int_default, TokId.op_comma, TokId.int_default, TokId.whitespace, 
        TokId.int_unsigned, TokId.op_comma, TokId.int_unsigned, TokId.whitespace, 
        TokId.int_long, TokId.op_comma, TokId.int_long, TokId.whitespace, 
        TokId.int_unsigned_long, TokId.op_comma, TokId.int_unsigned_long, TokId.whitespace, 
        TokId.float_default, TokId.op_comma, TokId.float_default, TokId.op_comma, TokId.float_default, TokId.op_comma, TokId.float_default, TokId.op_comma, TokId.float_default, TokId.whitespace, 
        TokId.float_short, TokId.op_comma, TokId.float_short, TokId.op_comma, TokId.float_short, TokId.op_comma, TokId.float_short, TokId.op_comma, TokId.float_short, TokId.whitespace, 
        TokId.float_long, TokId.op_comma, TokId.float_long, TokId.op_comma, TokId.float_long, TokId.op_comma, TokId.float_long, TokId.op_comma, TokId.float_long, TokId.whitespace, 
        TokId.float_default_im, TokId.op_comma, TokId.float_default_im, TokId.op_comma, TokId.float_default_im, TokId.op_comma, TokId.float_default_im, TokId.op_comma, TokId.float_default_im, TokId.whitespace, 
        TokId.float_short_im, TokId.op_comma, TokId.float_short_im, TokId.op_comma, TokId.float_short_im, TokId.op_comma, TokId.float_short_im, TokId.op_comma, TokId.float_short_im, TokId.whitespace, 
        TokId.float_long_im, TokId.op_comma, TokId.float_long_im, TokId.op_comma, TokId.float_long_im, TokId.op_comma, TokId.float_long_im, TokId.op_comma, TokId.float_long_im, TokId.whitespace, 

        TokId.float_default, TokId.op_comma, TokId.float_long, TokId.op_comma, TokId.float_short, TokId.op_comma, TokId.float_long, TokId.whitespace, 

        TokId.str_unknown, TokId.whitespace, TokId.str_16, TokId.whitespace,
        TokId.str_unknown, TokId.whitespace,  //x"f0 2e 5624"
        TokId.str_32, TokId.whitespace, TokId.str_16, TokId.whitespace, TokId.str_8, TokId.whitespace,

        TokId.eof]);

    }

    long totalNew = 0;
    long totalDparse = 0;
    totalNew += benchmarkNewTokenizer();
    totalDparse += benchmarkDParseTokenizer();
    totalDparse += benchmarkDParseTokenizer();
    totalNew += benchmarkNewTokenizer();
    totalDparse += benchmarkDParseTokenizer();
    totalNew += benchmarkNewTokenizer();
    totalDparse += benchmarkDParseTokenizer();
    totalNew += benchmarkNewTokenizer();
    writeln("total time for newTokenizer=", totalNew, " dparse=", totalDparse);

    if (!LLVM.loaded) {
		writeln("Cannot load LLVM dll");
        readln();
        return 1;
    }

    writeln("press any key");
    readln();
    return 0;
    //return fibo(args);

}
