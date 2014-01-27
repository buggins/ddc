module ddc.lexer.LineStream;

import std.stream;
import ddc.lexer.exceptions;
import std.stdio;
import std.conv;

class LineStream
{
    string _filename;
    InputStream _stream;
    ubyte[] _buf;  // stream reading buffer
    dchar[] _sbuf; // line buffer
    size_t _pos; // reading position of stream buffer
    size_t _len; // number of bytes in stream buffer
    size_t _spos; // position inside sbuf, for writing characters
    size_t _slen; // length of sbuf
    size_t _lstart; // start of current line in sbuf
    int _line; // current line number
    bool _eof; // end of decoded file
    bool _eos; // end of stream
    dchar _lastLineLastChar;
    immutable int BUFFER_SIZE = 4096;
    immutable int LINE_BUFFER_SIZE = 4096;
    enum EncodingType {
        ASCII,
        UTF8,
        UTF16BE,
        UTF16LE,
        UTF32BE,
        UTF32LE
    };
    EncodingType _encoding = EncodingType.ASCII;

    public this(InputStream stream, string filename)
    {
        _filename = filename;
        _stream = stream;
        _buf = new ubyte[BUFFER_SIZE];
        _eof = _eos = false;
        _pos = 0;
        _len = _stream.read(_buf);
        if (_buf[0] == 0xEF && _buf[1] == 0xEF && _buf[2] == 0xEF) {
            _encoding = EncodingType.UTF8;
            _pos += 3;
        } else if (_buf[0] == 0x00 && _buf[1] == 0x00 && _buf[2] == 0xFE && _buf[3] == 0xFF) {
            _encoding = EncodingType.UTF32BE;
            _pos += 4;
        } else if (_buf[0] == 0xFF && _buf[1] == 0xFE && _buf[2] == 0x00 && _buf[3] == 0x00) {
            _encoding = EncodingType.UTF32LE;
            _pos += 4;
        } else if (_buf[0] == 0xFE && _buf[1] == 0xFF) {
            _encoding = EncodingType.UTF16BE;
            _pos += 2;
        } else if (_buf[0] == 0xFF && _buf[1] == 0xFE) {
            _encoding = EncodingType.UTF16BE;
            _pos += 2;
        }
        _sbuf = new dchar[LINE_BUFFER_SIZE];
        _spos = 0;
        _slen = LINE_BUFFER_SIZE;
        _line = 0;
        _lstart = 0;
        _lastLineLastChar = 0;
    }

    public EncodingType encoding() {
        return _encoding;
    }
    public int line() {
        return _line;
    }
    private void fillBuf() {
        for (size_t i = _pos; i < _len; i++) {
            _buf[i - _pos] = _buf[i];
        }
        _len -= _pos;
        _pos = 0;
        if (!_stream.eof()) {
            _len += _stream.read(_buf[_pos .. $]);
        } else {
            _eos = true;
        }
    }

    // throw invalid code point exception
    private void invalidCodepoint() {
        throw new SourceEncodingException("Invalid codepoint", _filename, _line, _spos - _lstart);
    }

    private void convertChars() {
        if (_eof && _pos >= _len) {
            _eof = true;
            return;
        }
        // push characters to sbuf
        switch (_encoding) {
            case EncodingType.ASCII:
                while (_pos < _len && _spos < _slen) {
                    ubyte ch0 = _buf[_pos];
                    if (ch0 & 0x80)
                        invalidCodepoint();
                    if (ch0 == 0x1A || ch0 == 0x00) {
                        _eof = true;
                        break;
                    }
                    _sbuf[_spos++] = ch0;
                    _pos++;
                    if (ch0 == 0x0D || ch0 == 0x0A  || ch0 == 0x2028  || ch0 == 0x2029)
                        break;
                }
                break;
            case EncodingType.UTF8:
                while (_pos < _len && _spos < _slen) {
                    ptrdiff_t bleft = _len - _pos;
                    int bread = 0;
                    dchar ch0 = _buf[_pos];
                    dchar ch = 0;
                    if (!(ch0 & 0x80)) {
                        // 0x00..0x7F single byte
                        ch = ch0;
                        bread = 1;
                    } if ((ch0 & 0xE0) == 0xC0) {
                        // two bytes 110xxxxx 10xxxxxx
                        if (bleft < 2)
                            break;
                        dchar ch1 = _buf[_pos + 1];
                        if ((ch1 & 0xC0) != 0x80)
                            invalidCodepoint();
                        ch = ((ch0 & 0x1F) << 6) | ((ch1 & 0x3F));
                        bread = 2;
                    } if ((ch0 & 0xF0) == 0xE0) {
                        // three bytes 1110xxxx 10xxxxxx 10xxxxxx
                        if (bleft < 3)
                            break;
                        dchar ch1 = _buf[_pos + 1];
                        dchar ch2 = _buf[_pos + 2];
                        if ((ch1 & 0xC0) != 0x80 || (ch2 & 0xC0) != 0x80)
                            invalidCodepoint();
                        ch = ((ch0 & 0x0F) << 12) | ((ch1 & 0x1F) << 6) | ((ch2 & 0x3F));
                        bread = 3;
                    } if ((ch0 & 0xF8) == 0xF0) {
                        // four bytes 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                        if (bleft < 4)
                            break;
                        dchar ch1 = _buf[_pos + 1];
                        dchar ch2 = _buf[_pos + 2];
                        dchar ch3 = _buf[_pos + 3];
                        if ((ch1 & 0xC0) != 0x80 || (ch2 & 0xC0) != 0x80 || (ch3 & 0xC0) != 0x80)
                            invalidCodepoint();
                        ch = ((ch0 & 0x07) << 18) | ((ch1 & 0x3F) << 12) | ((ch2 & 0x3F) << 6) | ((ch3 & 0x3F));
                        bread = 4;
                    } if ((ch0 & 0xFC) == 0xF8) {
                        // five bytes 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
                        if (bleft < 5)
                            break;
                        dchar ch1 = _buf[_pos + 1];
                        dchar ch2 = _buf[_pos + 2];
                        dchar ch3 = _buf[_pos + 3];
                        dchar ch4 = _buf[_pos + 4];
                        if ((ch1 & 0xC0) != 0x80 || (ch2 & 0xC0) != 0x80 || (ch3 & 0xC0) != 0x80 || (ch4 & 0xC0) != 0x80)
                            invalidCodepoint();
                        ch = ((ch0 & 0x03) << 24) | ((ch1 & 0x3F) << 18) | ((ch2 & 0x3F) << 12) | ((ch3 & 0x3F) << 6) | ((ch4 & 0x3F));
                        bread = 5;
                    } if ((ch0 & 0xFE) == 0xFC) {
                        // six bytes 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
                        if (bleft < 6)
                            break;
                        dchar ch1 = _buf[_pos + 1];
                        dchar ch2 = _buf[_pos + 2];
                        dchar ch3 = _buf[_pos + 3];
                        dchar ch4 = _buf[_pos + 4];
                        dchar ch5 = _buf[_pos + 5];
                        if ((ch1 & 0xC0) != 0x80 || (ch2 & 0xC0) != 0x80 || (ch3 & 0xC0) != 0x80 || (ch4 & 0xC0) != 0x80 || (ch5 & 0xC0) != 0x80)
                            invalidCodepoint();
                        ch = ((ch0 & 0x01) << 30) | ((ch1 & 0x3F) << 24) | ((ch2 & 0x3F) << 18) | ((ch3 & 0x3F) << 12) | ((ch4 & 0x3F) << 6) | ((ch5 & 0x3F));
                        bread = 5;
                    }
                    if (!bread && _eos)
                        invalidCodepoint();
                    if (ch == 0x1A || ch == 0x00) {
                        _eof = true;
                        break;
                    }
                    _pos += bread;
                    _sbuf[_spos++] = ch;
                    if (ch == 0x0D || ch == 0x0A  || ch == 0x2028  || ch == 0x2029)
                        break;
                }
                break;
            case EncodingType.UTF16BE:
                // TODO
                while (_pos < _len - 1) {
                }
                break;
            case EncodingType.UTF16LE:
                // TODO
                while (_pos < _len - 1) {
                }
                break;
            case EncodingType.UTF32BE:
                // TODO
                while (_pos < _len - 3) {
                }
                break;
            case EncodingType.UTF32LE:
                // TODO
                while (_pos < _len - 3) {
                }
                break;
            default:
                throw new SourceEncodingException("Unsupported source encoding", _filename, 1, 0);
        }
    }
    private void checkLineSpace() {
        if (_spos < _slen - LINE_BUFFER_SIZE / 2) {
            if (_lstart > _slen / 2) {
                // move line content to beginning of line buffer
                for (size_t i = _lstart; i < _spos; i++)
                    _sbuf[i - _lstart] = _sbuf[i];
                _spos -= _lstart;
                _lstart = 0;
            }
            if (_spos > _slen - LINE_BUFFER_SIZE / 2) {
                // increase line buffer size
                _slen *= 2;
                _sbuf.length = _slen;
            }
        }
    }
    // returns line, or null for end of file
    public dchar[] readLine() {
        _line++;
        dchar lastChar = 0;
        for (;;) {
	        // fill buffer
	        if (_len - _pos < 4 && !_eos)
	            fillBuf();
	        // check space for line
	        checkLineSpace();
	        // convert chars
	        convertChars();
            writeln("_buffer: " ~ to!string(_lstart) ~ " : " ~ to!string(_spos));
            lastChar = _spos > _lstart ? _sbuf[_spos - 1] : 0;
            if (lastChar == 0x0A && _spos == _lstart + 1 && _lastLineLastChar == 0x0D) {
                writeln("skipping 0A after 0D at " ~ to!string(_spos));
                // it's second part of 0x0D 0x0A line ending - skip
                _lstart++;
                _lastLineLastChar = 0;
                if (_eof)
                    break;
                continue;
            }
            if (lastChar == 0x0D || lastChar == 0x0A  || lastChar == 0x2028  || lastChar == 0x2029 || _eof)
                break;
        }
        size_t lstart = _lstart;
        size_t lend = _spos;
        writeln("buffer: " ~ to!string(lstart) ~ " : " ~ to!string(lend));
        if (lastChar == 0x0D || lastChar == 0x0A  || lastChar == 0x2028  || lastChar == 0x2029)
            lend = _spos - 1;
        _lastLineLastChar = lastChar;
        if (lend == _lstart && _eof)
            return null; // eof
        _lstart = _spos;
        // return current line w/o 
//        if (lstart == lend)
//            return ""d;
        return _sbuf[cast(int)lstart .. cast(int)lend];
    }
}

unittest {
    import std.stdio;
    import std.conv;
    import std.utf;
    string fname = "C:\\projects\\d\\ddc\\ddclexer\\src\\ddc\\lexer\\LineStream.d";
    std.stream.File f = new std.stream.File(fname);

    scope(exit) f.close();
    try {
        LineStream lines = new LineStream(f, fname);
	    for (;;) {
		    dchar[] s = lines.readLine();
	        if (s is null)
	            break;
		    writeln("line " ~ to!string(lines.line()) ~ ":" ~ toUTF8(s));
	    }
    } catch (Exception e) {
        writeln("Exception " ~ e.stringof);
    }
    writeln("EOF");
}
