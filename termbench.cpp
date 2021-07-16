// SPDX-License-Identifier: GPL-3.0-or-later

#if defined(_WIN32)
#include <windows.h>
#include <intrin.h>
#endif

#if !defined(_WIN32)
#include <cpuid.h>
#include <immintrin.h>
#include <unistd.h>
#include <cstdint>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#endif

#if defined(__APPLE__)
#include <util.h>
#elif !defined(_WIN32)
#include <pty.h>
#endif

#include <stdio.h>
#include <chrono>
#include <optional>
#include <cstring>

#define VERSION_NAME "TermMarkV1"

#define VT_SYNC_START   "\033[?2026h"
#define VT_SYNC_END     "\033[?2026l"
#define VT_CLEAR_SCREEN "\033[H\033[J"

#define ArrayCount(Array) (sizeof(Array) / sizeof((Array)[0]))

struct buffer
{
    int MaxCount;
    int Count;
    char *Data;
};

enum class UserAction
{
	Idle, 					// no user input happenedc
	Exit,  					// Escape
	ToggleWritePerLine, 	// F1
	ToggleColorPerFrame, 	// F2
	ToggleSync, 			// F3
	ToggleAltScreen, 		// F4
	ResetStats,
};

static char NumberTable[256][4];

static void AppendChar(buffer *Buffer, char Char)
{
    if(Buffer->Count < Buffer->MaxCount) Buffer->Data[Buffer->Count++] = Char;
}

static void AppendString(buffer *Buffer, char const *String)
{
    while(*String) AppendChar(Buffer, *String++);
}

static void AppendDecimal(buffer *Buffer, int Value)
{
    if(Value < 0)
    {
        AppendChar(Buffer, '-');
        Value = -Value;
    }

    int Remains = Value;
    for(int Divisor = 1000000000;
        Divisor > 0;
        Divisor /= 10)
    {
        int Digit = Remains / Divisor;
        Remains -= Digit*Divisor;

        if(Digit || (Value != Remains) || (Divisor == 1))
        {
            AppendChar(Buffer, (char)('0' + Digit));
        }
    }
}

static void AppendGoto(buffer *Buffer, int X, int Y)
{
    AppendString(Buffer, "\x1b[");
    AppendDecimal(Buffer, Y);
    AppendString(Buffer, ";");
    AppendDecimal(Buffer, X);
    AppendString(Buffer, "H");
}

static void AppendColor(buffer *Buffer, int IsForeground, int unsigned Red, int unsigned Green, int unsigned Blue)
{
#if 0
    AppendString(Buffer, IsForeground ? "\x1b[38;2;" : "\x1b[48;2;");
    AppendString(Buffer, NumberTable[Red & 0xff]);
    AppendChar(Buffer, ';');
    AppendString(Buffer, NumberTable[Green & 0xff]);
    AppendChar(Buffer, ';');
    AppendString(Buffer, NumberTable[Blue & 0xff]);
    AppendChar(Buffer, 'm');
#else
    AppendString(Buffer, IsForeground ? "\x1b[38:2:" : "\x1b[48:2:");
    AppendString(Buffer, NumberTable[Red & 0xff]);
    AppendChar(Buffer, ':');
    AppendString(Buffer, NumberTable[Green & 0xff]);
    AppendChar(Buffer, ':');
    AppendString(Buffer, NumberTable[Blue & 0xff]);
    AppendChar(Buffer, 'm');
#endif
}

static void AppendStat(buffer *Buffer, char const *Name, int Value, char const *Suffix = "")
{
    AppendString(Buffer, Name);
    AppendString(Buffer, ": ");
    AppendDecimal(Buffer, Value);
    AppendString(Buffer, Suffix);
    AppendString(Buffer, "  ");
}

#define MAX_TERM_WIDTH 4096
#define MAX_TERM_HEIGHT 4096
static char TerminalBuffer[256+16*MAX_TERM_WIDTH*MAX_TERM_HEIGHT];

struct TerminalSize {
	unsigned short lines;
	unsigned short columns;
};

class TermBench {
public:
	virtual ~TermBench()
    {
        if (altScreenEnabled_)
        {
            setAltScreen(false);
        }
        printf("\nGood bye.\n");
    }

	virtual bool virtualTerminalSupport() const = 0;
	virtual void write(char const* _data, size_t _n) = 0;
	virtual UserAction tryGetUserAction() = 0;
	virtual TerminalSize terminalSize() const = 0;

    void setAltScreen(bool _enabled)
    {
        if (altScreenEnabled_ == _enabled)
            return;

        if (_enabled)
            fprintf(stdout, "Enabling alternate screen\n");

        fprintf(stdout, "\033[?1047%c", _enabled ? 'h' : 'l');
        fflush(stdout);

        if (!_enabled)
            fprintf(stdout, "Disabled alternate screen\n");

        altScreenEnabled_ = _enabled;
    }

protected:
    bool altScreenEnabled_ = false;
};

class perf_stat
{
public:
	virtual ~perf_stat() = default;

	virtual void begin_frame() = 0;
	virtual void end_frame(unsigned _nextBytecount, unsigned long long _markAccum) = 0;

	virtual void reset()
	{
		TermMarkAccum = 0;
		TermMark = 0;
	}

	virtual void tick_prep() = 0;
	virtual void tick_write() = 0;
	virtual void tick_read() = 0;

public:
    int TermMark = 0;
    int StatPercent = 0;

    int PrepMS = 0;
    int WriteMS = 0;
    int ReadMS = 0;
    int TotalMS = 0;

    int long long TermMarkAccum = 0;
};

void run(TermBench& _bench, perf_stat& _perf);

#if defined(_WIN32) // {{{
using large_integer_t = LARGE_INTEGER;

class win_perf_stat: public perf_stat
{
	large_integer_t A; // prep time
	large_integer_t B; // write time
	large_integer_t C; // read time
	large_intsize_t D: // total time
    large_integer_t Freq;
    large_integer_t AverageMark = {};
	unsigned NextByteCount;
	unsigned ByteCount = 0;

	void tick_prep() override { QueryPerformanceCounter(&B); }
	void tick_write() override { QueryPerformanceCounter(&C); }
	void tick_read() override { QueryPerformanceCounter(&D); }

	void begin_frame() override
	{
        NextByteCount = 0;

        QueryPerformanceCounter(&A);

        if(TermMarkAccum == 0)
        {
            AverageMark = A;
        }
        else
        {
            int long long AvgMS = 1000 * (A.QuadPart - AverageMark.QuadPart) / Freq.QuadPart;
            int long long StatMS = 10000;

            if(AvgMS > StatMS)
            {
                TermMark = (int)(1000*(TermMarkAccum / 1024) / AvgMS);
                AverageMark = A;
                TermMarkAccum = 0;
            }

            StatPercent = (int)(100*AvgMS / StatMS);
        }
	}

	void reset() override
	{
		perf_stat::reset();
	}

	void end_frame(unsigned _nextByteCount, unsigned long long _markAccum) override
	{
		ByteCount = _nextByteCount;
        PrepMS = GetMS(A.QuadPart, B.QuadPart, Freq.QuadPart);
        WriteMS = GetMS(B.QuadPart, C.QuadPart, Freq.QuadPart);
        ReadMS = GetMS(C.QuadPart, D.QuadPart, Freq.QuadPart);
        TotalMS = GetMS(A.QuadPart, D.QuadPart, Freq.QuadPart);
		TermMarkAccum += _markAccum;
	}

	win_perf_stat()
	{
        QueryPerformanceCounter(&A);
		QueryPerformanceFrequency(&Freq);
	}

	static int GetMS(int long long Start, int long long End, int long long Frequency)
	{
		int Result = (int)(1000*(End - Start) / Frequency);
		return Result;
	}
};

class WinTerminalIO: public TermBench
{
private:
	HANDLE TerminalIn;
	HANDLE TerminalOut;
	bool vtSupported_;
	TerminalSize size_;

public:
	WinTerminalIO()
	{
		DWORD WinConMode = 0;
		DWORD EnableVirtualTerminalProcessing = 0x0004;

		TerminalIn = GetStdHandle(STD_INPUT_HANDLE);
		TerminalOut = GetStdHandle(STD_OUTPUT_HANDLE);
		vtSupported_ = (GetConsoleMode(TerminalOut, &WinConMode) &&
									  SetConsoleMode(TerminalOut, (WinConMode & ~(ENABLE_ECHO_INPUT|ENABLE_LINE_INPUT)) |
													 EnableVirtualTerminalProcessing));
		updateTerminalSize();
	}

	void setSynchronizedOutput(bool _enabled)
	{
		// not supported
	}

	void updateTerminalSize()
	{
		CONSOLE_SCREEN_BUFFER_INFO ConsoleInfo;
		GetConsoleScreenBufferInfo(TerminalOut, &ConsoleInfo);
		auto Width = ConsoleInfo.srWindow.Right - ConsoleInfo.srWindow.Left;
		auto Height = ConsoleInfo.srWindow.Bottom - ConsoleInfo.srWindow.Top;
        if (Width > MAX_TERM_WIDTH) Width = MAX_TERM_WIDTH;
        if (Height > MAX_TERM_HEIGHT) Height = MAX_TERM_HEIGHT;
		size_ = TerminalSize{ Height, Width };
	}

	~WinTerminalIO()
	{
	}

	TerminalSize terminalSize() const
	{
		return size_;
	}

	bool virtualTerminalSupport() const override;
	{
		return vtSupported_;
	}

	void write(char const* _data, size_t _n) override
	{
		WriteConsoleA(TerminalOut, _data, _n, 0, 0);
	}

	UserAction tryGetUserAction() override
	{
        while (WaitForSingleObject(TerminalIn, 0) == WAIT_OBJECT_0)
        {
            INPUT_RECORD Record;
            DWORD RecordCount = 0;
            ReadConsoleInput(TerminalIn, &Record, 1, &RecordCount);
            if (RecordCount)
            {
                if ((Record.EventType == KEY_EVENT) &&
                    (Record.Event.KeyEvent.bKeyDown) &&
                    (Record.Event.KeyEvent.wRepeatCount == 1))
                {
                    switch (Record.Event.KeyEvent.wVirtualKeyCode)
                    {
                        case VK_ESCAPE:
							return UserAction::Exit;
                        case VK_F1:
							return UserAction::ToggleWritePerLine;
                        case VK_F2:
							return UserAction::ToggleColorPerFrame;
						case VK_F3:
							return UserAction::ToggleSync;
                        case VK_F4:
							return UserAction::ToggleAltScreen;
                    }
                }
                else if (Record.EventType == WINDOW_BUFFER_SIZE_EVENT)
                {
					updateTerminalSize();
					return UserAction::ResetStats;
                }
            }
        }
	}
};

extern "C" void mainCRTStartup(void)
{
    char CPU[65] = {};
    for(int SegmentIndex = 0;
        SegmentIndex < 3;
        ++SegmentIndex)
    {
        __cpuid((int *)(CPU + 16*SegmentIndex), 0x80000002 + SegmentIndex);
    }

	WinTerminalIO io{};
	win_perf_stat perf{};

	run(io, perf);
}
#endif
// }}}
#if !defined(_WIN32) // {{{
class UnixTerminalIO: public TermBench
{
	TerminalSize size_;
	struct termios backupTermios_;
	static UnixTerminalIO* singleton_;
    bool altScreen_ = false;

public:
	UnixTerminalIO()
	{
		singleton_ = this;

		winsize w{};
		ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
		size_.columns = w.ws_col;
		size_.lines = w.ws_row;

		termios raw;
		tcgetattr(STDIN_FILENO, &raw);
		backupTermios_ = raw;
		raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
		raw.c_oflag |= (ONLCR);
		raw.c_cflag |= (CS8);
		raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
		raw.c_cc[VMIN] = 1;
		raw.c_cc[VTIME] = 0;
		tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);

		fprintf(stdout, "\033[?25l"); // hide cursor
		fflush(stdout);

		signal(SIGINT, &onSignal);
		// TODO: react on SIGWINCH for terminal resize events
	}

	static void onSignal(int)
	{
		singleton_->~UnixTerminalIO();
	}

	~UnixTerminalIO()
	{
		tcsetattr(STDIN_FILENO, TCSAFLUSH, &backupTermios_);
		fprintf(stdout, "\033[?25h"); // show cursor
		fflush(stdout);
	}

	bool virtualTerminalSupport() const override
	{
		return true;
	}

	TerminalSize terminalSize() const override
	{
		return size_;
	}

	void write(char const* _data, size_t _n) override
	{
		[[maybe_unused]] auto nwritten = ::write(STDOUT_FILENO, _data, _n);
	}

	UserAction tryGetUserAction() override
	{
		fd_set in;
		fd_set out;
		fd_set err;
		FD_ZERO(&in);
		FD_ZERO(&out);
		FD_ZERO(&err);
		FD_SET(STDIN_FILENO, &in);
		timeval tv{};
		tv.tv_sec = 0;
		tv.tv_usec = 0;

		int rv = select(STDIN_FILENO + 1, &in, &out, &err, &tv);
		if (rv <= 0)
			return UserAction::Idle;

		char buf[32];
		int nread = read(STDIN_FILENO, buf, sizeof(buf));
		buf[nread] = 0;

		if (strcmp(buf, "\033OP") == 0) // F1
			return UserAction::ToggleWritePerLine;
		if (strcmp(buf, "\033OQ") == 0) // F2
			return UserAction::ToggleColorPerFrame;
		if (strcmp(buf, "\033OR") == 0) // F3
			return UserAction::ToggleSync;
		if (strcmp(buf, "\033OS") == 0) // F4
			return UserAction::ToggleAltScreen;
		if (strcmp(buf, "\033") == 0) // ESC....
			return UserAction::Exit;
		if (strcmp(buf, "q") == 0)
			return UserAction::Exit;

		return UserAction::Idle;
	}
};

UnixTerminalIO* UnixTerminalIO::singleton_ = 0;

class unix_perf_stat: public perf_stat
{
public:
	void begin_frame() override
	{
		// TODO
	}

	void reset() override
	{
	}

	void tick_prep() override
	{
	}

	void tick_write() override
	{
	}

	void tick_read() override
	{
	}

	void end_frame(unsigned _nextBytecount, unsigned long long _markAccum) override
	{
	}
};

int main(int argc, char const* argv[])
{
    for(int Num = 0; Num < 256; ++Num)
    {
        buffer NumBuf = {sizeof(NumberTable[Num]), 0, NumberTable[Num]};
        AppendDecimal(&NumBuf, Num);
        AppendChar(&NumBuf, 0);
    }

	UnixTerminalIO io{};
	unix_perf_stat perf{};
	run(io, perf);
}
#endif // }}}

void run(TermBench& _bench, perf_stat& _perf)
{
    int Running = true;
    int FrameIndex = 0;
    int WritePerLine = false;
    int ColorPerFrame = false;

	bool SyncOutput = false;

    bool LastAltScreen = false;
    bool AltScreen = true;

	// TODO: replace with perf_stat
    int PrepMS = 0;
    int WriteMS = 0;
    int ReadMS = 0;
    int TotalMS = 0;

	TerminalSize terminalSize = _bench.terminalSize();

    int ByteCount = 0;

    while (Running)
    {
        if (AltScreen != LastAltScreen)
        {
            _bench.setAltScreen(AltScreen);
            LastAltScreen = AltScreen;
        }

		_perf.begin_frame();

        buffer Frame = {sizeof(TerminalBuffer), 0, TerminalBuffer};

		if (SyncOutput)
			AppendString(&Frame, VT_SYNC_START);

        if (AltScreen)
            AppendString(&Frame, VT_CLEAR_SCREEN);

		unsigned NextByteCount = 0;
        for(unsigned short Y = 0; Y <= terminalSize.lines; ++Y)
        {
            AppendGoto(&Frame, 1, 1 + Y);
            for(unsigned short X = 0; X <= terminalSize.columns; ++X)
            {
                if (!ColorPerFrame)
                {
                    int BackRed = FrameIndex + Y + X;
                    int BackGreen = FrameIndex + Y;
                    int BackBlue = FrameIndex;

                    int ForeRed = FrameIndex;
                    int ForeGreen = FrameIndex + Y;
                    int ForeBlue = FrameIndex + Y + X;

                    AppendColor(&Frame, false, BackRed, BackGreen, BackBlue);
                    AppendColor(&Frame, true, ForeRed, ForeGreen, ForeBlue);
                }

                char Char = 'a' + (char)((FrameIndex + X + Y) % ('z' - 'a'));
                AppendChar(&Frame, Char);
            }

            if(WritePerLine)
            {
                NextByteCount += Frame.Count;
				_bench.write(Frame.Data, Frame.Count);
                Frame.Count = 0;
            }
        }

        AppendColor(&Frame, false, 0, 0, 0);
        AppendColor(&Frame, true, 255, 255, 255);
        AppendGoto(&Frame, 1, 1);
        AppendStat(&Frame, "Glyphs", (terminalSize.lines * terminalSize.columns) / 1024, "k");
        AppendStat(&Frame, "Bytes", ByteCount / 1024, "kb");
        AppendStat(&Frame, "Frame", FrameIndex);

        if(!WritePerLine) AppendStat(&Frame, "Prep", PrepMS, "ms");
        AppendStat(&Frame, "Write", WriteMS, "ms");
        AppendStat(&Frame, "Read", ReadMS, "ms");
        AppendStat(&Frame, "Total", TotalMS, "ms");

        AppendGoto(&Frame, 1, 2);
        AppendString(&Frame, WritePerLine ? "[F1]:write per line " : "[F1]:write per frame ");
        AppendString(&Frame, ColorPerFrame ? "[F2]:color per frame " : "[F2]:color per char ");

        if(!WritePerLine) // {{{ print stats
        {
            AppendGoto(&Frame, 1, 3);
            if(_perf.TermMark)
            {
                AppendStat(&Frame, VERSION_NAME, _perf.TermMark, ColorPerFrame ? "kg/s" : "kcg/s");
                AppendString(&Frame, "(");
#if defined(_WIN32)
                AppendString(&Frame, CPU);
                AppendString(&Frame, " Win33 ");
#endif
                AppendString(&Frame, _bench.virtualTerminalSupport() ? "VTS)" : "NO VTS REPORTED)");
            }
            else
            {
                AppendStat(&Frame, "(collecting", _perf.StatPercent, "%)");
            }
        } // }}}

		_perf.tick_prep();

		if (SyncOutput)
			AppendString(&Frame, VT_SYNC_END);

        NextByteCount += Frame.Count;
		_bench.write(Frame.Data, Frame.Count);
		_perf.tick_write();

        int ResetStats = false;
		switch (_bench.tryGetUserAction())
		{
			case UserAction::Idle:
				break;
			case UserAction::Exit:
				Running = !Running;
				break;
			case UserAction::ToggleColorPerFrame:
				ColorPerFrame = !ColorPerFrame;
				ResetStats = true;
				break;
			case UserAction::ToggleWritePerLine:
				WritePerLine = !WritePerLine;
				ResetStats = true;
				break;
			case UserAction::ToggleSync:
				SyncOutput = !SyncOutput;
				ResetStats = true;
				break;
			case UserAction::ToggleAltScreen:
				AltScreen = !AltScreen;
				ResetStats = true;
				break;
			case UserAction::ResetStats:
				ResetStats = true;
		}

		_perf.tick_read();

		_perf.end_frame(NextByteCount, terminalSize.lines * terminalSize.columns);
        ByteCount = NextByteCount;
        ++FrameIndex;

        if(ResetStats)
			_perf.reset();
    }
}

//
// NOTE(casey): Support definitions for CRT-less Visual Studio and CLANG
//
#if defined(_WIN32)

#if !defined(__clang__) && !defined(__GNUC__)
#undef function
#pragma function(memset)
#endif
extern "C" void *memset(void *DestInit, int Source, size_t Size)
{
    unsigned char *Dest = (unsigned char *)DestInit;
    while(Size--) *Dest++ = (unsigned char)Source;

    return(DestInit);
}

#if !defined(__clang__) && !defined(__GNUC__)
#pragma function(memcpy)
#endif
extern "C" void *memcpy(void *DestInit, void const *SourceInit, size_t Size)
{
    unsigned char *Source = (unsigned char *)SourceInit;
    unsigned char *Dest = (unsigned char *)DestInit;
    while(Size--) *Dest++ = *Source++;

    return(DestInit);
}

#endif
