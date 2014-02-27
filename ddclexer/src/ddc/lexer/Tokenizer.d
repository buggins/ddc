module ddc.lexer.Tokenizer;

import ddc.lexer.LineStream;
import ddc.lexer.exceptions;

import std.stdio;
import std.datetime;
import std.conv;

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
	OP
}

// table for fast checking of UniversalAlpha (as per ISO/IEC 9899:1999 Annex E) OR a..z OR A..Z OR _
// max code is 0xd7ff
//1728
const uint[1728] UNIVERSAL_ALPHA_FLAGS = [
    0x00000000,0x00000000,0x87fffffe,0x07fffffe,0x00000000,0x04a00400,0xff7fffff,0xff7fffff,// 0000-00ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xfc3fffff,// 0100-01ff
    0x00ffffff,0x00000000,0xffff0000,0xffffffff,0xffffffff,0xe9ff01ff,0x00030003,0x0000001f,// 0200-02ff
    0x00000000,0x00000000,0x00000000,0x04000000,0xffffd740,0xfffffffb,0x547f7fff,0x000ffffd,// 0300-03ff
    0xffffdffe,0xffffffff,0xdffeffff,0xffffffff,0xffff0003,0xffffffff,0xffff199f,0x033fcfff,// 0400-04ff
    0x00000000,0xfffe0000,0x027fffff,0xfffffffe,0x000000ff,0xbbff0000,0xffff0006,0x000707ff,// 0500-05ff
    0x00000000,0x07fffffe,0x0007ffff,0xffff03ff,0xffffffff,0x7cffffff,0x1fff7fff,0x03ff3de0,// 0600-06ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 0700-07ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 0800-08ff
    0xffffffee,0xe3ffffff,0xff073fff,0x0000ffcf,0xfff99fee,0xc3c5fdff,0xb000399f,0x0003ffcf,// 0900-09ff
    0xfff987e4,0xc36dfdff,0x5e003987,0x0010ffc0,0xfffbafee,0xe3edfdff,0x00013bbf,0x0000ffc1,// 0a00-0aff
    0xfff99fee,0xe3cdfdff,0xb000398f,0x0000ffc3,0xd63dc7ec,0xc3bfc718,0x00003dc7,0x0000ff80,// 0b00-0bff
    0xfffddfee,0xc3effdff,0x00003ddf,0x0000ffc3,0xfffddfec,0xc3effdff,0x40003ddf,0x0000ffc3,// 0c00-0cff
    0xfffddfec,0xc3fffdff,0x00003dcf,0x0000ffc3,0x00000000,0x00000000,0x00000000,0x00000000,// 0d00-0dff
    0xfffffffe,0x07ffffff,0x0fffffff,0x00000000,0xfef02596,0x3bff6cae,0x33ff3f5f,0x00000000,// 0e00-0eff
    0x03000001,0xc2afffff,0xfffffeff,0xfffe03ff,0xfebf0fdf,0x02fe3fff,0x00000000,0x00000000,// 0f00-0fff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0xffffffff,0xffff003f,0x007fffff,// 1000-10ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1100-11ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1200-12ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1300-13ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1400-14ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1500-15ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1600-16ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1700-17ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1800-18ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1900-19ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1a00-1aff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1b00-1bff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1c00-1cff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 1d00-1dff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0x0fffffff,0xffffffff,0xffffffff,0x03ffffff,// 1e00-1eff
    0x3f3fffff,0xffffffff,0xaaff3f3f,0x3fffffff,0xffffffff,0x5fdfffff,0x0fcf1fdc,0x1fdc1fff,// 1f00-1fff
    0x00000000,0x80000000,0x00000001,0x80000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2000-20ff
    0x3f2ffc84,0x01fbfd50,0x00000000,0xffffffff,0x00000007,0x00000000,0x00000000,0x00000000,// 2100-21ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2200-22ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2300-23ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2400-24ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2500-25ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2600-26ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2700-27ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2800-28ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2900-29ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2a00-2aff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2b00-2bff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2c00-2cff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2d00-2dff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2e00-2eff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 2f00-2fff
    0x000000e0,0x000003fe,0xfffffffe,0xffffffff,0x180fffff,0xfffffffe,0xffffffff,0x187fffff,// 3000-30ff
    0xffffffe0,0x00001fff,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3100-31ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3200-32ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3300-33ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3400-34ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3500-35ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3600-36ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3700-37ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3800-38ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3900-39ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3a00-3aff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3b00-3bff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3c00-3cff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3d00-3dff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3e00-3eff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 3f00-3fff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4000-40ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4100-41ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4200-42ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4300-43ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4400-44ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4500-45ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4600-46ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4700-47ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4800-48ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4900-49ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4a00-4aff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4b00-4bff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4c00-4cff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// 4d00-4dff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 4e00-4eff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 4f00-4fff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5000-50ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5100-51ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5200-52ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5300-53ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5400-54ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5500-55ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5600-56ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5700-57ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5800-58ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5900-59ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5a00-5aff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5b00-5bff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5c00-5cff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5d00-5dff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5e00-5eff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 5f00-5fff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6000-60ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6100-61ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6200-62ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6300-63ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6400-64ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6500-65ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6600-66ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6700-67ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6800-68ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6900-69ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6a00-6aff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6b00-6bff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6c00-6cff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6d00-6dff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6e00-6eff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 6f00-6fff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7000-70ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7100-71ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7200-72ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7300-73ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7400-74ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7500-75ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7600-76ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7700-77ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7800-78ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7900-79ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7a00-7aff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7b00-7bff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7c00-7cff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7d00-7dff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7e00-7eff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 7f00-7fff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8000-80ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8100-81ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8200-82ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8300-83ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8400-84ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8500-85ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8600-86ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8700-87ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8800-88ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8900-89ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8a00-8aff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8b00-8bff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8c00-8cff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8d00-8dff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8e00-8eff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 8f00-8fff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9000-90ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9100-91ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9200-92ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9300-93ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9400-94ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9500-95ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9600-96ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9700-97ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9800-98ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9900-99ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9a00-9aff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9b00-9bff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9c00-9cff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9d00-9dff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// 9e00-9eff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0x0000003f,0x00000000,0x00000000,// 9f00-9fff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a000-a0ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a100-a1ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a200-a2ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a300-a3ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a400-a4ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a500-a5ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a600-a6ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a700-a7ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a800-a8ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// a900-a9ff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// aa00-aaff
    0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,// ab00-abff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// ac00-acff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// ad00-adff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// ae00-aeff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// af00-afff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b000-b0ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b100-b1ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b200-b2ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b300-b3ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b400-b4ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b500-b5ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b600-b6ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b700-b7ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b800-b8ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// b900-b9ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// ba00-baff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// bb00-bbff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// bc00-bcff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// bd00-bdff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// be00-beff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// bf00-bfff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c000-c0ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c100-c1ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c200-c2ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c300-c3ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c400-c4ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c500-c5ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c600-c6ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c700-c7ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c800-c8ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// c900-c9ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// ca00-caff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// cb00-cbff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// cc00-ccff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// cd00-cdff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// ce00-ceff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// cf00-cfff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// d000-d0ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// d100-d1ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// d200-d2ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// d300-d3ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// d400-d4ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// d500-d5ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,// d600-d6ff
    0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0x0000000f,0x00000000,0x00000000// d700-d7ff
];

// returns true if character is A..Z, a..z, _ or universal alpha
public bool isUniversalAlpha(dchar ch) pure nothrow {
//	uint index = ch >> 5;
//	uint mask = 1 << (ch & 31);
//	uint value = UNIVERSAL_ALPHA_FLAGS[index];
//	return (ch <= 0xd7ff && (value & mask) != 0);
	return (ch <= 0xd7ff && (UNIVERSAL_ALPHA_FLAGS[ch >> 5] & (1 << (ch & 31))));
}

public bool isIdentStartChar(dchar ch) pure nothrow {
	return isUniversalAlpha(ch);
}

public bool isIdentMiddleChar(dchar ch) pure nothrow {
	return (ch >= '0' && ch <='9') || isUniversalAlpha(ch);
}
	
immutable bool ENABLE_DUMP_UNIVERSAL_ALPHA_TABLE = false;
static if (ENABLE_DUMP_UNIVERSAL_ALPHA_TABLE) {
		bool r(dchar ch, wchar v) pure nothrow {
			return ch == v;
		}
		
		bool r(dchar ch, wchar v1, wchar v2) pure nothrow {
			return ch >= v1 && ch <= v2;
		}

		bool isUniversalAlphaSlow(dchar c)  pure nothrow {
			return 
				// Latin: 00AA, 00BA, 00C0−00D6, 00D8−00F6, 00F8−01F5, 01FA−0217,
				// 0250−02A8, 1E00−1E9B, 1EA0−1EF9, 207F
				r(c, 0xAA) || r(c, 0x00BA) || r(c, 0x00C0,0x00D6) || r(c, 0x00D8,0x00F6) || r(c, 0x00F8,0x01F5) || r(c, 0x01FA,0x0217)
				|| r(c, 0x0250,0x02A8) || r(c, 0x1E00,0x1E9B) || r(c, 0x1EA0,0x1EF9) || r(c, 0x207F)
				//Greek: 0386, 0388−038A, 038C, 038E−03A1, 03A3−03CE, 03D0−03D6,
				//03DA, 03DC, 03DE, 03E0, 03E2−03F3, 1F00−1F15, 1F18−1F1D,
				//1F20−1F45, 1F48−1F4D, 1F50−1F57, 1F59, 1F5B, 1F5D,
				//1F5F−1F7D, 1F80−1FB4, 1FB6−1FBC, 1FC2−1FC4, 1FC6−1FCC,
				//1FD0−1FD3, 1FD6−1FDB, 1FE0−1FEC, 1FF2−1FF4, 1FF6−1FFC
				|| r(c, 0x0386) || r(c, 0x0388,0x038A) || r(c, 0x038C) || r(c, 0x038E,0x03A1) || r(c, 0x03A3,0x03CE) || r(c, 0x03D0,0x03D6)
				|| r(c, 0x03DA) || r(c, 0x03DC) || r(c, 0x03DE) || r(c, 0x03E0) || r(c, 0x03E2,0x03F3) || r(c, 0x1F00,0x1F15) || r(c, 0x1F18,0x1F1D)
				|| r(c, 0x1F20,0x1F45) || r(c, 0x1F48,0x1F4D) || r(c, 0x1F50,0x1F57) || r(c, 0x1F59) || r(c, 0x1F5B) || r(c, 0x1F5D)
				|| r(c, 0x1F5F,0x1F7D) || r(c, 0x1F80,0x1FB4) || r(c, 0x1FB6,0x1FBC) || r(c, 0x1FC2,0x1FC4) || r(c, 0x1FC6,0x1FCC)
				|| r(c, 0x1FD0,0x1FD3) || r(c, 0x1FD6,0x1FDB) || r(c, 0x1FE0,0x1FEC) || r(c, 0x1FF2,0x1FF4) || r(c, 0x1FF6,0x1FFC)
				//Cyrillic: 0401−040C, 040E−044F, 0451−045C, 045E−0481, 0490−04C4,
				//04C7−04C8, 04CB−04CC, 04D0−04EB, 04EE−04F5, 04F8−04F9
				|| r(c, 0x0401,0x040C) || r(c, 0x040E,0x044F) || r(c, 0x0451,0x045C) || r(c, 0x045E,0x0481) || r(c, 0x0490,0x04C4)
				|| r(c, 0x04C7,0x04C8) || r(c, 0x04CB,0x04CC) || r(c, 0x04D0,0x04EB) || r(c, 0x04EE,0x04F5) || r(c, 0x04F8,0x04F9)
				//Armenian: 0531−0556, 0561−0587
				|| r(c, 0x0531,0x0556) || r(c, 0x0561,0x0587)
				//Hebrew: 05B0−05B9, 05BB−05BD, 05BF, 05C1−05C2, 05D0−05EA,
				//05F0−05F2
				|| r(c, 0x05B0,0x05B9) || r(c, 0x05BB,0x05BD) || r(c, 0x05BF) || r(c, 0x05C1,0x05C2) || r(c, 0x05D0,0x05EA)
				|| r(c, 0x05F0,0x05F2)
				//Arabic: 0621−063A, 0640−0652, 0670−06B7, 06BA−06BE, 06C0−06CE,
				//06D0−06DC, 06E5−06E8, 06EA−06ED
				|| r(c, 0x0621,0x063A) || r(c, 0x0640,0x0652) || r(c, 0x0670,0x06B7) || r(c, 0x06BA,0x06BE) || r(c, 0x06C0,0x06CE)
				|| r(c, 0x06D0,0x06DC) || r(c, 0x06E5,0x06E8) || r(c, 0x06EA,0x06ED)
				//Devanagari: 0901−0903, 0905−0939, 093E−094D, 0950−0952, 0958−0963
				|| r(c, 0x0901,0x0903) || r(c, 0x0905,0x0939) || r(c, 0x093E,0x094D) || r(c, 0x0950,0x0952) || r(c, 0x0958,0x0963)
				//Bengali: 0981−0983, 0985−098C, 098F−0990, 0993−09A8, 09AA−09B0,
				//09B2, 09B6−09B9, 09BE−09C4, 09C7−09C8, 09CB−09CD,
				//09DC−09DD, 09DF−09E3, 09F0−09F1
				|| r(c, 0x0981,0x0983) || r(c, 0x0985,0x098C) || r(c, 0x098F,0x0990) || r(c, 0x0993,0x09A8) || r(c, 0x09AA,0x09B0)
				|| r(c, 0x09B2) || r(c, 0x09B6,0x09B9) || r(c, 0x09BE,0x09C4) || r(c, 0x09C7,0x09C8) || r(c, 0x09CB,0x09CD)
				|| r(c, 0x09DC,0x09DD) || r(c, 0x09DF,0x09E3) || r(c, 0x09F0,0x09F1)
				//Gurmukhi: 0A02, 0A05−0A0A, 0A0F−0A10, 0A13−0A28, 0A2A−0A30,
				//0A32−0A33, 0A35−0A36, 0A38−0A39, 0A3E−0A42, 0A47−0A48,
				//0A4B−0A4D, 0A59−0A5C, 0A5E, 0A74
				|| r(c, 0x0A02) || r(c, 0x0A05,0x0A0A) || r(c, 0x0A0F,0x0A10) || r(c, 0x0A13,0x0A28) || r(c, 0x0A2A,0x0A30)
				|| r(c, 0x0A32,0x0A33) || r(c, 0x0A35,0x0A36) || r(c, 0x0A38,0x0A39) || r(c, 0x0A3E,0x0A42) || r(c, 0x0A47,0x0A48)
				|| r(c, 0x0A4B,0x0A4D) || r(c, 0x0A59,0x0A5C) || r(c, 0x0A5E) || r(c, 0x0A74)
				//Gujarati: 0A81−0A83, 0A85−0A8B, 0A8D, 0A8F−0A91, 0A93−0AA8,
				//0AAA−0AB0, 0AB2−0AB3, 0AB5−0AB9, 0ABD−0AC5,
				//0AC7−0AC9, 0ACB−0ACD, 0AD0, 0AE0
				|| r(c, 0x0A81,0x0A83) || r(c, 0x0A85,0x0A8B) || r(c, 0x0A8D) || r(c, 0x0A8F,0x0A91) || r(c, 0x0A93,0x0AA8)
				|| r(c, 0x0AAA,0x0AB0) || r(c, 0x0AB2,0x0AB3) || r(c, 0x0AB5,0x0AB9) || r(c, 0x0ABD,0x0AC5)
				|| r(c, 0x0AC7,0x0AC9) || r(c, 0x0ACB,0x0ACD) || r(c, 0x0AD0) || r(c, 0x0AE0)
				// Oriya: 0B01−0B03, 0B05−0B0C, 0B0F−0B10, 0B13−0B28, 0B2A−0B30,
				//0B32−0B33, 0B36−0B39, 0B3E−0B43, 0B47−0B48, 0B4B−0B4D,
				//0B5C−0B5D, 0B5F−0B61
				|| r(c, 0x0B01,0x0B03) || r(c, 0x0B05,0x0B0C) || r(c, 0x0B0F,0x0B10) || r(c, 0x0B13,0x0B28) || r(c, 0x0B2A,0x0B30)
				|| r(c, 0x0B32,0x0B33) || r(c, 0x0B36,0x0B39) || r(c, 0x0B3E,0x0B43) || r(c, 0x0B47,0x0B48) || r(c, 0x0B4B,0x0B4D)
				|| r(c, 0x0B5C,0x0B5D) || r(c, 0x0B5F,0x0B61)
				//Tamil: 0B82−0B83, 0B85−0B8A, 0B8E−0B90, 0B92−0B95, 0B99−0B9A,
				//0B9C, 0B9E−0B9F, 0BA3−0BA4, 0BA8−0BAA, 0BAE−0BB5,
				//0BB7−0BB9, 0BBE−0BC2, 0BC6−0BC8, 0BCA−0BCD
				|| r(c, 0x0B82,0x0B83) || r(c, 0x0B85,0x0B8A) || r(c, 0x0B8E,0x0B90) || r(c, 0x0B92,0x0B95) || r(c, 0x0B99,0x0B9A)
				|| r(c, 0x0B9C) || r(c, 0x0B9E,0x0B9F) || r(c, 0x0BA3,0x0BA4) || r(c, 0x0BA8,0x0BAA) || r(c, 0x0BAE,0x0BB5)
				|| r(c, 0x0BB7,0x0BB9) || r(c, 0x0BBE,0x0BC2) || r(c, 0x0BC6,0x0BC8) || r(c, 0x0BCA,0x0BCD)
				//Telugu: 0C01−0C03, 0C05−0C0C, 0C0E−0C10, 0C12−0C28, 0C2A−0C33,
				//0C35−0C39, 0C3E−0C44, 0C46−0C48, 0C4A−0C4D, 0C60−0C61
				|| r(c, 0x0C01,0x0C03) || r(c, 0x0C05,0x0C0C) || r(c, 0x0C0E,0x0C10) || r(c, 0x0C12,0x0C28) || r(c, 0x0C2A,0x0C33)
				|| r(c, 0x0C35,0x0C39) || r(c, 0x0C3E,0x0C44) || r(c, 0x0C46,0x0C48) || r(c, 0x0C4A,0x0C4D) || r(c, 0x0C60,0x0C61)
				//Kannada: 0C82−0C83, 0C85−0C8C, 0C8E−0C90, 0C92−0CA8, 0CAA−0CB3,
				//0CB5−0CB9, 0CBE−0CC4, 0CC6−0CC8, 0CCA−0CCD, 0CDE,
				//0CE0−0CE1
				|| r(c, 0x0C82,0x0C83) || r(c, 0x0C85,0x0C8C) || r(c, 0x0C8E,0x0C90) || r(c, 0x0C92,0x0CA8) || r(c, 0x0CAA,0x0CB3)
				|| r(c, 0x0CB5,0x0CB9) || r(c, 0x0CBE,0x0CC4) || r(c, 0x0CC6,0x0CC8) || r(c, 0x0CCA,0x0CCD) || r(c, 0x0CDE)
				|| r(c, 0x0CE0,0x0CE1)
				//Malayalam: 0D02−0D03, 0D05−0D0C, 0D0E−0D10, 0D12−0D28, 0D2A−0D39,
				//0D3E−0D43, 0D46−0D48, 0D4A−0D4D, 0D60−0D61
				|| r(c, 0x0D02,0x0D03) || r(c, 0x0D05,0x0D0C) || r(c, 0x0D0E,0x0D10) || r(c, 0x0D12,0x0D28) || r(c, 0x0D2A,0x0D39)
				|| r(c, 0xD3E,0x0D43) || r(c, 0x0D46,0x0D48) || r(c, 0x0D4A,0x0D4D) || r(c, 0x0D60,0x0D61)
				//Thai: 0E01−0E3A, 0E40−0E5B
				|| r(c, 0x0E01,0x0E3A) || r(c, 0x0E40,0x0E5B)
				//Lao: 0E81−0E82, 0E84, 0E87−0E88, 0E8A, 0E8D, 0E94−0E97,
				//0E99−0E9F, 0EA1−0EA3, 0EA5, 0EA7, 0EAA−0EAB,
				//0EAD−0EAE, 0EB0−0EB9, 0EBB−0EBD, 0EC0−0EC4, 0EC6,
				//0EC8−0ECD, 0EDC−0EDD
				|| r(c, 0x0E81,0x0E82) || r(c, 0x0E84) || r(c, 0x0E87,0x0E88) || r(c, 0x0E8A) || r(c, 0x0E8D) || r(c, 0x0E94,0x0E97)
				|| r(c, 0x0E99,0x0E9F) || r(c, 0x0EA1,0x0EA3) || r(c, 0x0EA5) || r(c, 0x0EA7) || r(c, 0x0EAA,0x0EAB)
				|| r(c, 0x0EAD,0x0EAE) || r(c, 0x0EB0,0x0EB9) || r(c, 0x0EBB,0x0EBD) || r(c, 0x0EC0,0x0EC4) || r(c, 0x0EC6)
				|| r(c, 0x0EC8,0x0ECD) || r(c, 0x0EDC,0x0EDD)
				//Tibetan: 0F00, 0F18−0F19, 0F35, 0F37, 0F39, 0F3E−0F47, 0F49−0F69,
				//0F71−0F84, 0F86−0F8B, 0F90−0F95, 0F97, 0F99−0FAD,
				//0FB1−0FB7, 0FB9
				|| r(c, 0x0F00) || r(c, 0x0F18,0x0F19) || r(c, 0x0F35) || r(c, 0x0F37) || r(c, 0x0F39) || r(c, 0x0F3E,0x0F47) || r(c, 0x0F49,0x0F69)
				|| r(c, 0x0F71,0x0F84) || r(c, 0x0F86,0x0F8B) || r(c, 0x0F90,0x0F95) || r(c, 0x0F97) || r(c, 0x0F99,0x0FAD)
				|| r(c, 0x0FB1,0x0FB7) || r(c, 0x0FB9)
				//Georgian: 10A0−10C5, 10D0−10F6
				|| r(c, 0x10A0,0x10C5) || r(c, 0x10D0,0x10F6)
				//Hiragana: 3041−3093, 309B−309C
				|| r(c, 0x3041,0x3093) || r(c, 0x309B,0x309C)
				//Katakana: 30A1−30F6, 30FB−30FC
				|| r(c, 0x30A1,0x30F6) || r(c, 0x30FB,0x30FC)
				//Bopomofo: 3105−312C
				|| r(c, 0x3105,0x312C)
				//CJK Unified Ideographs: 4E00−9FA5
				|| r(c, 0x4E00,0x9FA5)
				//Hangul: AC00−D7A3
				|| r(c, 0xAC00,0xD7A3)
				//Digits: 0660−0669, 06F0−06F9, 0966−096F, 09E6−09EF, 0A66−0A6F,
				//0AE6−0AEF, 0B66−0B6F, 0BE7−0BEF, 0C66−0C6F, 0CE6−0CEF,
				//0D66−0D6F, 0E50−0E59, 0ED0−0ED9, 0F20−0F33
				|| r(c, 0x0660,0x0669) || r(c, 0x06F0,0x06F9) || r(c, 0x0966,0x096F) || r(c, 0x09E6,0x09EF) || r(c, 0x0A66,0x0A6F)
				|| r(c, 0x0AE6,0x0AEF) || r(c, 0x0B66,0x0B6F) || r(c, 0x0BE7,0x0BEF) || r(c, 0x0C66,0x0C6F) || r(c, 0x0CE6,0x0CEF)
				|| r(c, 0x0D66,0x0D6F) || r(c, 0x0E50,0x0E59) || r(c, 0x0ED0,0x0ED9) || r(c, 0x0F20,0x0F33)
				//Special characters: 00B5, 00B7, 02B0−02B8, 02BB, 02BD−02C1, 02D0−02D1,
				//02E0−02E4, 037A, 0559, 093D, 0B3D, 1FBE, 203F−2040, 2102,
				//2107, 210A−2113, 2115, 2118−211D, 2124, 2126, 2128, 212A−2131,
				//2133−2138, 2160−2182, 3005−3007, 3021−3029
				|| r(c, 0x00B5) || r(c, 0x00B7) || r(c, 0x02B0,0x02B8) || r(c, 0x02BB) || r(c, 0x02BD,0x02C1) || r(c, 0x02D0,0x02D1)
				|| r(c, 0x2E0,0x02E4) || r(c, 0x037A) || r(c, 0x0559) || r(c, 0x093D) || r(c, 0x0B3D) || r(c, 0x1FBE) || r(c, 0x203F,0x2040) || r(c, 0x2102)
				|| r(c, 0x2107) || r(c, 0x210A,0x2113) || r(c, 0x2115) || r(c, 0x2118,0x211D) || r(c, 0x2124) || r(c, 0x2126) || r(c, 0x2128) || r(c, 0x212A,0x2131)
				|| r(c, 0x2133,0x2138) || r(c, 0x2160,0x2182) || r(c, 0x3005,0x3007) || r(c, 0x3021,0x3029)
				;
		}

}

unittest {
	
		
	static if (ENABLE_DUMP_UNIVERSAL_ALPHA_TABLE) {
		immutable uint itemsInRow = 8;
		
		uint maxAlpha = 0;
		for (uint i = 0; i < 0x10000; i++) {
			uint ch = i;
			if (isUniversalAlphaSlow(ch) || ch == '_' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))
				maxAlpha = i;
		}
		maxAlpha = (maxAlpha + itemsInRow * 32 - 1) / (itemsInRow * 32) * (itemsInRow * 32) - 1;
		writeln("// table for fast checking of UniversalAlpha (as per ISO/IEC 9899:1999 Annex E) OR a..z OR A..Z OR _");
		writefln("// max code is 0x%04x", maxAlpha);
		writeln("immutable uint[", (maxAlpha + 1) / 32,"] UNIVERSAL_ALPHA_FLAGS = [");
		for (uint i = 0; i <= maxAlpha; i += 32) {
			if ((i / 32) % itemsInRow  == 0)
				write("    ");
			uint flags = 0;
			for (uint j = 0; j < 32; j++) {
				uint ch = i + j;
				bool flag = isUniversalAlphaSlow(ch) || ch == '_' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
				if (flag)
					flags |= (1 << j);
			}
			writef("0x%08x", flags);
			if (i != maxAlpha / 32 * 32)
				write(",");
			if ((i / 32) % itemsInRow  == itemsInRow - 1)
				writefln("// %04x-%04x", i - itemsInRow * 32 + 1 + 31, i + 31);
		}
		writeln("];");
		
		for (uint ch = 0; ch < 0x100000; ch++) {
			bool flag = isUniversalAlphaSlow(ch) || ch == '_' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
			bool flag2 = isUniversalAlpha(ch);
			if (flag2 != flag) {
				isUniversalAlpha(ch);
				writefln("universalAlpha test failed for char %06x expeced %d actual %d", ch, flag ? 1 : 0, flag2 ? 1 : 0);
			}
			assert(flag2 == flag);
		}
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
	COLON, 		//    ;
	SEMICOLON, 	//    :
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

immutable dstring[] KEYWORD_STRINGS = [
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

public dstring getKeywordNameD(Keyword keyword) pure nothrow {
	return KEYWORD_STRINGS[keyword];
};

public Keyword findKeyword(Keyword start, Keyword end, dchar * name, uint len, ref uint pos) pure nothrow {
	for (Keyword i = start; i <= end; i++) {
		dstring s = KEYWORD_STRINGS[i];
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
			
class Token {
	protected TokenType _type;
	protected string _file;
	protected uint _line;
	protected uint _pos;
	public @property TokenType type() { return _type; }
	public @property string filename() { return _file; }
	public @property uint line() { return _line; }
	public @property uint pos() { return _pos; }
	public @property dchar[] text() { return null; }
	public @property dchar literalType() { return 0; }
	public @property ulong intValue() { return 0; }
	public @property bool isUnsigned() { return false; }
	public @property ulong isLong() { return false; }
	public @property OpCode opCode() { return OpCode.NONE; }
	public @property Keyword keyword() { return Keyword.NONE; }
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

class OpToken : Token {
	OpCode _op;
	public @property override OpCode opCode() { return _op; }
	public @property void opCode(OpCode op) { _op = op; }
	public @property override dchar[] text() { return cast(dchar[])getOpNameD(_op); }
	this() {
		super(TokenType.OP);
	}
	this(string file, uint line, uint pos) {
		super(TokenType.OP, file, line, pos);
	}
	override public Token clone() {
		return new OpToken(_file, _line, _pos);
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
	this(string file, uint line, uint pos) {
		super(TokenType.KEYWORD, file, line, pos);
	}
	override public Token clone() {
		return new KeywordToken(_file, _line, _pos);
	}
}

// do we need comment text?

class CommentToken : Token {
	dchar[] _text;
	public @property override dchar[] text() { return _text; }
	public @property void text(dchar[] text) { _text = text; }
	this() {
		super(TokenType.COMMENT);
	}
	this(string file, uint line, uint pos, dchar[] text) {
		super(TokenType.COMMENT, file, line, pos);
		_text = text;
	}
	override public Token clone() {
		return new CommentToken(_file, _line, _pos, _text);
	}
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
	this(string file, uint line, uint pos, dchar[] text, dchar type) {
		super(TokenType.STRING, file, line, pos);
		_text = text;
		_literalType = type;
	}
	override public Token clone() {
		return new StringLiteralToken(_file, _line, _pos, _text.dup, _literalType);
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
	this(string file, uint line, uint pos, ulong value, bool unsignedFlag, bool longFlag) {
		super(TokenType.INTEGER, file, line, pos);
		_value = value;
		_unsigned = unsignedFlag;
		_long = longFlag;
	}
	override public Token clone() {
		return new IntegerLiteralToken(_file, _line, _pos, _value, _unsigned, _long);
	}
}

class IdentToken : Token {
	dchar[] _text;
	public @property override dchar[] text() { return _text; }
	public void setText(dchar[] text) { _text = text; }
	this() {
		super(TokenType.IDENTIFIER);
	}
	this(string file, uint line, uint pos, dchar[] text) {
		super(TokenType.IDENTIFIER, file, line, pos);
		_text = text;
	}
	override public Token clone() {
		return new IdentToken(_file, _line, _pos, _text.dup);
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
	void reset() {
		len = 0;
	}
}

class Tokenizer
{
	LineStream _lineStream;
	dchar[] _lineText;
	uint _line; // current line number
	uint _len; // current line length
	uint _pos; // current line read position
	uint _state; // tokenizer state
	
	immutable dchar EOF_CHAR = 0x001A;
	immutable dchar EOL_CHAR = 0x000A;
	
	WhiteSpaceToken _sharedWhiteSpaceToken = new WhiteSpaceToken();
	CommentToken _sharedCommentToken = new CommentToken();
	StringLiteralToken _sharedStringLiteralToken = new StringLiteralToken();
	IdentToken _sharedIdentToken = new IdentToken();
	OpToken _sharedOpToken = new OpToken();
	KeywordToken _sharedKeywordToken = new KeywordToken();
	IntegerLiteralToken _sharedIntegerToken = new IntegerLiteralToken();
	StringAppender _stringLiteralAppender;
	StringAppender _commentAppender;
	StringAppender _identAppender;
	
	bool _enableCommentText = true;
	public void enableCommentText(bool enabled) {
		_enableCommentText = enabled;
	}
	
	this(LineStream lineStream) {
		_lineStream = lineStream;
		_sharedWhiteSpaceToken.setFile(_lineStream.filename);
		_sharedCommentToken.setFile(_lineStream.filename);
		_sharedStringLiteralToken.setFile(_lineStream.filename);
		_sharedIdentToken.setFile(_lineStream.filename);
		_sharedOpToken.setFile(_lineStream.filename);
		_sharedKeywordToken.setFile(_lineStream.filename);
		_sharedIntegerToken.setFile(_lineStream.filename);
		buildTime = Clock.currTime();
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
	
	dchar nextChar() {
		if (_lineText is null) {
			if (!nextLine()) {
				return EOF_CHAR;
			}
		}
		if (_pos >= _len)
			return EOL_CHAR;
		return _lineText[_pos++];
	}
	
	dchar peekChar() {
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
	
	Token processWhiteSpace(dchar firstChar) {
		uint line = _line;
		uint pos = _pos - 1;
		for (;;) {
			uint i = _pos;
			for (; i < _len; i++) {
				dchar ch = _lineText[i];
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
		dchar[] text;
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
	Token processWysiwygString(dchar ch) {
		_pos++;
		// TODO:
		return null;
	}
	
	Token processIdent() {
		_sharedIdentToken.setPos(_line, _pos - 1);
		_identAppender.reset();
		uint startPos = _pos - 1;
		uint endPos = _len;
		for (uint i = _pos; i < _len; i++) {
			dchar ch = _lineText[i];
			if (!isIdentMiddleChar(ch)) {
				endPos = i;
				break;
			}
		}
		_pos = endPos;
		_sharedIdentToken.setText(_lineText[startPos .. endPos]);
		return _sharedIdentToken;
	}

	Token processIntegerSuffix() {
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
			parserError("Unexpected character after number");
		return _sharedIntegerToken;
	}
	
	Token processBinaryNumber() {
		_sharedIntegerToken.setPos(_line, _pos - 1);
		_pos++;
		if (_pos >= _len)
			parserError("Unexpected end of line in binary number");
		int digits = 0;
		ulong number = 0;
		uint i = _pos;
		for (;i < _len; i++) {
			dchar ch = _lineText[i];
			if (ch != '0' && ch != '1')
				break;
			number = number | ((ch == '1' ? 1 : 0) << digits);
			digits++;
		}
		_pos = i;
		if (digits > 64)
			parserError("number is too big");
		_sharedIntegerToken.setValue(number);
		return processIntegerSuffix();
	}

	Token processHexNumber() {
		_sharedIntegerToken.setPos(_line, _pos - 1);
		_pos++;
		if (_pos >= _len)
			parserError("Unexpected end of line in hex number");
		int digits = 0;
		ulong number = 0;
		return null;
	}
	
	Token processOctNumber() {
		_pos++;
		if (_pos >= _len)
			parserError("Unexpected end of line in oct number");
		int digits = 0;
		ulong number = 0;
		return null;
	}
	
	Token processDecNumber(dchar ch) {
		return null;
	}
		
	void parserError(string msg) {
		throw new ParserException(msg, _lineStream.filename, _line, _pos);
	}

	Keyword detectKeyword(dchar ch) {
		if (ch > 'z')
			return Keyword.NONE;
		uint len = _len - _pos;
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
	OpCode detectOp(dchar ch) nothrow {
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
			//	COLON, 		//    ;
			case ';':
				return OpCode.COLON;
			//	SEMICOLON, 	//    :
			case ':':
				return OpCode.SEMICOLON;
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
	
	Token processDoubleQuotedOrWysiwygString(dchar delimiter) {
		bool wysiwyg = (delimiter == 'r' || delimiter == '`');
		//writeln("processDoubleQuotedString()");
		_sharedStringLiteralToken.setPos(_line, _pos - 1);
		_stringLiteralAppender.reset();
		if (delimiter == 'r') {
			_pos++;
			delimiter = '\"';
		}
		dchar type = 0;
		for (;;) {
			uint i = _pos;
			uint endPos = uint.max;
			for(; i < _len; i++) {
				if (_lineText[i] == delimiter && (i == 0 || _lineText[i - 1] != '\\')) {
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
		dchar t = 0;
		if (_pos < _len) {
			dchar ch = _lineText[_pos];
			if (ch == 'c' || ch == 'w' || ch == 'd')
				t = ch;
			else if (isIdentMiddleChar(ch))
				parserError("Unexpected character after string literal");
		}
		if (t != 0) {
			if (type != 0 && t != type)
				parserError("Cannot concatenate strings of different type");
			type = t;
		}
		if (!wysiwyg) {
			// no escape processing
			_sharedStringLiteralToken.setText(_stringLiteralAppender.get(), type);
			return _sharedStringLiteralToken;
		}
		// TODO: process escape sequences
		_sharedStringLiteralToken.setText(_stringLiteralAppender.get(), type);
		return _sharedStringLiteralToken;
	}

	SysTime buildTime;
	
	//	string literal of the date of compilation "mmm dd yyyy"
	dstring formatBuildDate() {
		// TODO: provide proper format
		return to!dstring(buildTime);
	}
	
	//	string literal of the time of compilation "hh:mm:ss"
	dstring formatBuildTime() {
		// TODO: provide proper format
		return to!dstring(buildTime);
	}
	
	//	string literal of the date and time of compilation "www mmm dd hh:mm:ss yyyy"
	dstring formatBuildTimestamp() {
		// TODO: provide proper format
		return to!dstring(buildTime);
	}
	
	immutable dstring VERSION = "0.1";
	immutable dstring VENDOR = "coolreader.org";
	
	Token makeSpecialTokenString(dstring str, uint pos) {
		_sharedStringLiteralToken.setPos(_line, pos);
		_sharedStringLiteralToken.setText(cast(dchar[])str, 0);
		return _sharedStringLiteralToken;
	}
	
	Token processSpecialToken(Keyword keyword, uint pos) {
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
				parserError("Unexpected token");
		}
		return null;
	}
	
	// returns next token (clone it if you want to store for future usage, otherwise it may be overwritten by further nextToken() calls).
	public Token nextToken() {
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
		if (ch == '\"')
			return processDoubleQuotedOrWysiwygString(ch);
		if (ch == 'x' && next == '\"')
			return processHexString();
		if (ch == 'q' && next == '\"')
			return processDelimitedString();
		if ((ch == 'r' && next == '\"') || (ch == '`'))
			return processDoubleQuotedOrWysiwygString(ch);
		uint oldPos = _pos - 1;
		
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
						_sharedKeywordToken.setPos(_line, oldPos);
						_sharedKeywordToken.keyword = keyword;
						return _sharedKeywordToken;
				}
			}
			return processIdent();
		}
		OpCode op = detectOp(ch);
		if (op != OpCode.NONE) {
			_sharedOpToken.setPos(_line, oldPos);
			_sharedOpToken.opCode = op;
			return _sharedOpToken;
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
			writeln("", token.line, ":", token.pos, "\t", token.type, "\t", token.opCode, "\t", token.keyword, "\t\"", toUTF8(token.text), "\"");
	    }
    } catch (Exception e) {
        writeln("Exception " ~ e.toString);
    }
}
