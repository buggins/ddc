module ddc.lexer.LineStream;

import std.stream;
import ddc.lexer.exceptions;
import std.stdio;
import std.conv;

class LineStream {
	public enum EncodingType {
        ASCII,
        UTF8,
        UTF16BE,
        UTF16LE,
        UTF32BE,
        UTF32LE
    };

    InputStream _stream;
	string _filename;
    ubyte[] _buf;  // stream reading buffer
    uint _pos; // reading position of stream buffer
    uint _len; // number of bytes in stream buffer
	bool _streamEof; // true if input stream is in EOF state
	uint _line; // current line number
	
	uint _textPos; // start of text line in text buffer
	uint _textLen; // position of last filled char in text buffer + 1
	wchar[] _textBuf; // text buffer
	bool _eof; // end of file, no more lines
	
	@property string filename() { return _filename; }
	@property uint line() { return _line; }
	@property EncodingType encoding() { return _encoding; }
	
    immutable EncodingType _encoding;

	int _errorCode;
	string _errorMessage;
	uint _errorLine;
	uint _errorPos;

	protected this(InputStream stream, string filename, EncodingType encoding, ubyte[] buf, uint offset, uint len) {
		_filename = filename;
		_stream = stream;
		_encoding = encoding;
		_buf = buf;
		_len = len;
		_pos = offset;
		_streamEof = _stream.eof;
	}
	
	// returns slice of bytes available in buffer
	uint readBytes() {
		uint bytesLeft = _len - _pos;
		if (_streamEof || bytesLeft > QUARTER_BYTE_BUFFER_SIZE)
			return bytesLeft;
		if (_pos > 0) {
			for (uint i = 0; i < bytesLeft; i++)
				_buf[i] = _buf[i + _pos];
			_len = bytesLeft;
			_pos = 0;
		}
		uint bytesRead = cast(uint)_stream.read(_buf[_len .. BYTE_BUFFER_SIZE]);
		_len += bytesRead;
		_streamEof = _stream.eof;
		return _len - _pos; //_buf[_pos .. _len];
	}

	// when bytes consumed from byte buffer, call this method to update position
	void consumedBytes(uint count) {
		_pos += count;
	}
	
	// reserve text buffer for specified number of characters, and return pointer to first free character in buffer
	wchar * reserveTextBuf(uint len) {
		// create new text buffer if necessary
		if (_textBuf == null) {
			if (len < TEXT_BUFFER_SIZE)
				len = TEXT_BUFFER_SIZE;
			_textBuf = new wchar[len];
			return _textBuf.ptr;
		}
		uint spaceLeft = cast(uint)_textBuf.length - _textLen;
		if (spaceLeft >= len)
			return _textBuf.ptr + _textLen;
		// move text to beginning of buffer, if necessary
		if (_textPos > _textBuf.length / 2) {
			uint charCount = _textLen - _textPos;
			wchar * p = _textBuf.ptr;
			for (uint i = 0; i < charCount; i++)
				p[i] = p[i + _textPos];
			_textLen = charCount;
			_textPos = 0;
		}
		// resize buffer if necessary
		if (_textLen + len > _textBuf.length) {
			// resize buffer
			uint newsize = cast(uint)_textBuf.length * 2;
			if (newsize < _textLen + len)
				newsize = _textLen + len;
			_textBuf.length = newsize;
		}
		return _textBuf.ptr + _textLen;
	}
	
	void appendedText(uint len) {
		writeln("appended ", len, " chars of text"); //:", _textBuf[_textLen .. _textLen + len]);
		_textLen += len;
	}
	
	void setError(int code, string message, uint errorLine, uint errorPos) {
		_errorCode = code;
		_errorMessage = message;
	}
	
	// override to decode text
	abstract uint decodeText();
	
	immutable uint LINE_POSITION_UNDEFINED = uint.max;
	public wchar[] readLine() {
		if (_errorCode != 0) {
			writeln("error ", _errorCode, ": ", _errorMessage, " in line ", _errorLine);
			return null; // error detected
		}
		if (_eof) {
			writeln("EOF found");
			return null;
		}
		_line++;
		writeln("line ", _line, " textPos=", _textPos, " textLen=", _textLen, " streamEof=", _streamEof, ", textBuf.length", _textBuf.length);
		uint p = 0;
		uint eol = LINE_POSITION_UNDEFINED;
		uint eof = LINE_POSITION_UNDEFINED;
		uint lastchar = LINE_POSITION_UNDEFINED;
		do {
			if (_errorCode != 0) {
				//writeln("error ", _errorCode, ": ", _errorMessage, " in line ", _errorLine);
				return null; // error detected
			}
			uint charsLeft = _textLen - _textPos;
			if (p >= charsLeft) {
				uint decodedChars = decodeText();
				charsLeft = _textLen - _textPos;
				if (decodedChars == 0) {
					eol = charsLeft;
					eof = charsLeft;
					lastchar = charsLeft;
					break;
				} 
			}
			for (; p < charsLeft; p++) {
				wchar ch = _textBuf[_textPos + p];
				if (ch == 0x0D) {
					lastchar = p;
					if (p == charsLeft - 1) {
						// need one more char to check if it's 0D0A or just 0D eol
						//writeln("read one more char for 0D0A detection");
						decodeText();
						charsLeft = _textLen - _textPos;
					}
					wchar ch2 = (p < charsLeft - 1) ? _textBuf[_textPos + p + 1] : 0;
					if (ch2 == 0x0A)
						eol = p + 2;
					else
						eol = p + 1;
					break;
				} else if (ch == 0x0A || ch == 0x2028 || ch == 0x2029) {
					// single char eoln
					lastchar = p;
					eol = p + 1;
					break;
				} else if (ch == 0 || ch == 0x001A) {
					// eof
					//writeln("EOF char found");
					lastchar = p;
					eol = eof = p + 1;
					break;
				}
			}
		} while (eol == LINE_POSITION_UNDEFINED);
		uint lineStart = _textPos;
		uint lineEnd = _textPos + lastchar;
		_textPos += eol; // consume text
		if (eof != LINE_POSITION_UNDEFINED) {
			_eof = true;
			//writeln("Setting eof flag. lastchar=", lastchar, ", p=", p, ", lineStart=", lineStart);
			if (lineStart >= lineEnd) {
				//writeln("lineStart >= lineEnd -- treat as eof");
				return null; // eof
			}
		}
		// return slice with decoded line
		return _textBuf[lineStart .. lineEnd];
	}
	
	wchar[] _emptyString;
	immutable int TEXT_BUFFER_SIZE = 1024;
	immutable int BYTE_BUFFER_SIZE = 512;
	immutable int QUARTER_BYTE_BUFFER_SIZE = BYTE_BUFFER_SIZE / 4;
	
	// factory
	public static LineStream create(InputStream stream, string filename) {
		ubyte[] buf = new ubyte[BYTE_BUFFER_SIZE];
		buf[0] = buf[1] = buf[2]  = buf[3] = 0;
		if (!stream.isOpen)
			return null;
        uint len = cast(uint)stream.read(buf);
        if (buf[0] == 0xEF && buf[1] == 0xEF && buf[2] == 0xEF) {
			return new Utf8LineStream(stream, filename, buf, len);
        } else if (buf[0] == 0x00 && buf[1] == 0x00 && buf[2] == 0xFE && buf[3] == 0xFF) {
			return new Utf32beLineStream(stream, filename, buf, len);
        } else if (buf[0] == 0xFF && buf[1] == 0xFE && buf[2] == 0x00 && buf[3] == 0x00) {
			return new Utf32leLineStream(stream, filename, buf, len);
        } else if (buf[0] == 0xFE && buf[1] == 0xFF) {
			return new Utf16beLineStream(stream, filename, buf, len);
        } else if (buf[0] == 0xFF && buf[1] == 0xFE) {
			return new Utf32leLineStream(stream, filename, buf, len);
		} else {
			return new AsciiLineStream(stream, filename, buf, len);
		}
	}
}



class AsciiLineStream : LineStream {
	this(InputStream stream, string filename, ubyte[] buf, uint len) {
		super(stream, filename, EncodingType.ASCII, buf, 0, len);
	}	
	override uint decodeText() {
		uint bytesAvailable = readBytes();
		ubyte * bytes = _buf.ptr + _pos;
		if (bytesAvailable == 0)
			return 0; // nothing to decode
		uint len = bytesAvailable;
		ubyte* b = bytes;
		wchar* text = reserveTextBuf(len);
		assert(_textLen + len <= _textBuf.length);
		for (uint i = 0; i < len; i++) {
			assert(_textLen + i < _textBuf.length);
			ubyte ch = b[i];
			if (ch & 0x80) {
				// invalid character
				setError(1, "Invalid character", _line, _textLen - _textPos + i + 1);
				return 0;
			}
			text[i] = ch;
		}
		consumedBytes(len);
		appendedText(len);
		return len;
	}
	
}

class Utf8LineStream : LineStream {
	this(InputStream stream, string filename, ubyte[] buf, uint len) {
		super(stream, filename, EncodingType.UTF8, buf, 3, len);
	}	
	override uint decodeText() {
		return 0;
	}
}

class Utf16beLineStream : LineStream {
	this(InputStream stream, string filename, ubyte[] buf, uint len) {
		super(stream, filename, EncodingType.UTF16BE, buf, 2, len);
	}	
	override uint decodeText() {
		return 0;
	}
}

class Utf16leLineStream : LineStream {
	this(InputStream stream, string filename, ubyte[] buf, uint len) {
		super(stream, filename, EncodingType.UTF16LE, buf, 2, len);
	}	
	override uint decodeText() {
		return 0;
	}
}

class Utf32beLineStream : LineStream {
	this(InputStream stream, string filename, ubyte[] buf, uint len) {
		super(stream, filename, EncodingType.UTF32BE, buf, 4, len);
	}	
	override uint decodeText() {
		return 0;
	}
}

class Utf32leLineStream : LineStream {
	this(InputStream stream, string filename, ubyte[] buf, uint len) {
		super(stream, filename, EncodingType.UTF32LE, buf, 4, len);
	}	
	override uint decodeText() {
		return 0;
	}
}

/*
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
*/
unittest {
    import std.stdio;
    import std.conv;
    import std.utf;
	assert("" == null);
	assert("" !is null);
    //string fname = "C:\\projects\\d\\ddc\\ddclexer\\src\\ddc\\lexer\\LineStream.d";
    string fname = "/home/lve/src/d/ddc/ddclexer/" ~ __FILE__; //"/home/lve/src/d/ddc/ddclexer/src/ddc/lexer/Lexer.d";
	writeln("opening first file");
    std.stream.File f = new std.stream.File(fname);
	writeln("opening second file");
//    std.stream.File f2 = new std.stream.File(fname);
//	writeln("reading contents");
	scope(exit) { f.close(); }
    try {
        LineStream lines = LineStream.create(f, fname);
	    for (;;) {
//			writeln("reading line from reference file");
//			wchar[] referenceLine = null;
//			try {
//				referenceLine = f2.readLineW(); //f2.eof ? null : f2.readLineW();
//			} catch (Exception e) {
//				writeln("Exception ", e);
//			}
				
		    wchar[] s = lines.readLine();
	        if (s is null)
	            break;
//			if (!std.algorithm.equal(referenceLine, s)) {
//				writeln("Different lines: \n", s, "\n", referenceLine);
//			}
		    writeln("line " ~ to!string(lines.line()) ~ ":" ~ toUTF8(s));
	    }
    } catch (Exception e) {
        writeln("Exception " ~ e.stringof);
    }
    writeln("EOF");
}
// LAST LINE
