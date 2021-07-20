#if defined(_WIN32)
#include <windows.h>
#include <intrin.h>
#else
#include <cpuid.h>
#include <immintrin.h>
#include <unistd.h>
#include <cstdint>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
	#if defined(__APPLE__)
	#include <util.h>
	#else
	#include <pty.h>
	#endif
#endif

#include <array>
#include <chrono>
#include <cstring>
#include <optional>
#include <stdio.h>
#include <tuple>

#define VERSION_NAME "TermMarkV1"

#if 0
// legacy sequence
#define VT_SYNC_BEGIN "\033P=1s\033\\"
#define VT_SYNC_END   "\033P=2s\033\\"
#else
#define VT_SYNC_BEGIN "\033[?2026h" "\033P=1s\033\\"
#define VT_SYNC_END   "\033[?2026l" "\033P=2s\033\\"
#endif

#define ArrayCount(Array) (sizeof(Array) / sizeof((Array)[0]))

struct buffer
{
    int MaxCount;
    int Count;
    char *Data;
};

using std::array;
using std::tie;
using std::tuple;

enum class UserAction
{
	Idle, 					// no user input happenedc
	Exit,  					// Escape
	ToggleWritePerLine, 	// F1
	ToggleColorPerFrame, 	// F2
	ToggleSync, 			// F3
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

class TerminalIO {
public:
	virtual ~TerminalIO() = default;
	virtual bool virtualTerminalSupport() const = 0;
	virtual void write(char const* _data, size_t _n) = 0;
	virtual UserAction tryGetUserAction() = 0;

	virtual TerminalSize terminalSize() const = 0;
};

using StatResult = tuple<int, int, int, int>;

class perf_stat
{
public:
	virtual ~perf_stat() = default;

	virtual void begin_frame() = 0;
	virtual StatResult end_frame(unsigned _nextBytecount, unsigned long long _markAccum) = 0;

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

void run(TerminalIO& _io, perf_stat& _perf);

#if defined(_WIN32)
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

	StatResult end_frame(unsigned _nextByteCount, unsigned long long _markAccum) override
	{
		ByteCount = _nextByteCount;
        PrepMS = GetMS(A.QuadPart, B.QuadPart, Freq.QuadPart);
        WriteMS = GetMS(B.QuadPart, C.QuadPart, Freq.QuadPart);
        ReadMS = GetMS(C.QuadPart, D.QuadPart, Freq.QuadPart);
        TotalMS = GetMS(A.QuadPart, D.QuadPart, Freq.QuadPart);
		TermMarkAccum += _markAccum;
        return {PrepMS, WriteMS, ReadMS, TotalMS};
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

class WinTerminalIO: public TerminalIO
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
using large_integer_t = LARGE_INTEGER;

#else

class UnixTerminalIO: public TerminalIO // {{{
{
	TerminalSize size_;
	struct termios backupTermios_;
	static UnixTerminalIO* singleton_;

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
		if (strcmp(buf, "\033") == 0) // ESC....
			return UserAction::Exit;
		if (strcmp(buf, "q") == 0) // ESC....
			return UserAction::Exit;

		return UserAction::Idle;
	}
};
UnixTerminalIO* UnixTerminalIO::singleton_ = 0;
// }}}

using std::chrono::milliseconds;
using std::chrono::duration_cast;
using std::chrono::seconds;
using std::chrono::steady_clock;

class unix_perf_stat: public perf_stat // {{{
{
public:
    steady_clock::time_point A;
    std::chrono::milliseconds B, C, D;

	void begin_frame() override
	{
        A = steady_clock::now();
	}

	void tick_prep() override
	{
        B = duration_cast<milliseconds>(steady_clock::now() - A);
	}

	void tick_write() override
	{
        C = duration_cast<milliseconds>(steady_clock::now() - A) - B;
	}

	void tick_read() override
	{
        D = duration_cast<milliseconds>(steady_clock::now() - A) - C;
	}

	StatResult end_frame(unsigned _nextBytecount, unsigned long long _markAccum) override
	{
        return {
            B.count(),
            C.count(),
            D.count(),
            duration_cast<milliseconds>(steady_clock::now() - A).count()
        };
	}

	void reset() override
	{
        A = steady_clock::now();
        B = {};
        C = {};
        D = {};
	}
}; // }}}

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
#endif

void run(TerminalIO& _io, perf_stat& _perf)
{
    int Running = true;
    int FrameIndex = 0;
    int WritePerLine = false;
    int ColorPerFrame = false;
	bool SyncOutput = true;

	// TODO: replace with perf_stat
    int PrepMS = 0;
    int WriteMS = 0;
    int ReadMS = 0;
    int TotalMS = 0;

	TerminalSize terminalSize = _io.terminalSize();

    int ByteCount = 0;

    std::array<unsigned, 3> freqs{};
    auto lastFreqIndex = duration_cast<seconds>(steady_clock::now().time_since_epoch()).count() % freqs.size();

    while (Running)
    {
		_perf.begin_frame();

        auto const freqIndex = duration_cast<seconds>(steady_clock::now().time_since_epoch()).count() % freqs.size();
        if (freqIndex != lastFreqIndex)
        {
            freqs[freqIndex] = 0;
            lastFreqIndex = freqIndex;
        }
        freqs[freqIndex]++;

        buffer Frame = {sizeof(TerminalBuffer), 0, TerminalBuffer};

		if (SyncOutput)
			AppendString(&Frame, VT_SYNC_BEGIN);

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
				_io.write(Frame.Data, Frame.Count);
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
        AppendStat(&Frame, "W", WriteMS, "ms");
        AppendStat(&Frame, "R", ReadMS, "ms");
        AppendStat(&Frame, "T", TotalMS, "ms");

        AppendStat(&Frame, "Fi", freqIndex, "");
        // AppendStat(&Frame, "F1", freqs[0], "");
        // AppendStat(&Frame, "F2", freqs[1], "");
        // AppendStat(&Frame, "F3", freqs[2], "");
        unsigned long fps = 0;
        for (size_t i = 0; i < freqs.size(); ++i)
            if (freqIndex != i)
                fps += freqs[i];
        fps /= freqs.size() - 1;
        AppendStat(&Frame, "F", fps, "");

        AppendGoto(&Frame, 1, 2);
        AppendString(&Frame, WritePerLine ? "[F1]:write per line " : "[F1]:write per frame ");
        AppendString(&Frame, ColorPerFrame ? "[F2]:color per frame " : "[F2]:color per char ");
        AppendString(&Frame, SyncOutput ? "[F3]: synchronized output" : "[F3]:no synchronized output ");

        if(!WritePerLine) // {{{ print stats
        {
            AppendGoto(&Frame, 1, 3);
            if(_perf.TermMark)
            {
                AppendStat(&Frame, VERSION_NAME, _perf.TermMark, ColorPerFrame ? "kg/s" : "kcg/s");
                AppendString(&Frame, "(");
#if defined(_WIN32)
                AppendString(&Frame, CPU);
                AppendString(&Frame, " Win32 ");
#endif
                AppendString(&Frame, _io.virtualTerminalSupport() ? "VTS)" : "NO VTS REPORTED)");
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
		_io.write(Frame.Data, Frame.Count);
		_perf.tick_write();

        int ResetStats = false;
		// XXX Output:
		// bool WritePerLine
		// bool toggleColorPerFrame;
		switch (_io.tryGetUserAction())
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
			case UserAction::ResetStats:
				ResetStats = true;

		}

		_perf.tick_read();

        tie(PrepMS, WriteMS, ReadMS, TotalMS) = _perf.end_frame(NextByteCount, terminalSize.lines * terminalSize.columns);


        ByteCount = NextByteCount;
        ++FrameIndex;

        if(ResetStats)
			_perf.reset();
    }
}

//
// NOTE(casey): Support definitions for CRT-less Visual Studio and CLANG
//

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
