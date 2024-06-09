#ifndef TSF_INTERNAL_H_
#define TSF_INTERNAL_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef char tsf_fourcc[4];
typedef signed char tsf_s8;
typedef unsigned char tsf_u8;
typedef unsigned short tsf_u16;
typedef signed short tsf_s16;
typedef unsigned int tsf_u32;
typedef char tsf_char20[20];

struct tsf
{
	struct tsf_preset* presets;
	float* fontSamples;
	struct tsf_voice* voices;
	struct tsf_channels* channels;

	int presetNum;
	int voiceNum;
	int maxVoiceNum;
	unsigned int voicePlayIndex;

	enum TSFOutputMode outputmode;
	float outSampleRate;
	float globalGainDB;
	int* refCount;

    unsigned int fontSampleCount;
    struct tsf_sample * samples;
    int sampleNum;
};

enum { TSF_LOOPMODE_NONE, TSF_LOOPMODE_CONTINUOUS, TSF_LOOPMODE_SUSTAIN };

enum { TSF_SEGMENT_NONE, TSF_SEGMENT_DELAY, TSF_SEGMENT_ATTACK, TSF_SEGMENT_HOLD, TSF_SEGMENT_DECAY, TSF_SEGMENT_SUSTAIN, TSF_SEGMENT_RELEASE, TSF_SEGMENT_DONE };

struct tsf_hydra
{
	struct tsf_hydra_phdr *phdrs; struct tsf_hydra_pbag *pbags; struct tsf_hydra_pmod *pmods;
	struct tsf_hydra_pgen *pgens; struct tsf_hydra_inst *insts; struct tsf_hydra_ibag *ibags;
	struct tsf_hydra_imod *imods; struct tsf_hydra_igen *igens; struct tsf_hydra_shdr *shdrs;
	int phdrNum, pbagNum, pmodNum, pgenNum, instNum, ibagNum, imodNum, igenNum, shdrNum;
};

struct tsf_stream_memory { const char* buffer; unsigned int total, pos; };

union tsf_hydra_genamount { struct { tsf_u8 lo, hi; } range; tsf_s16 shortAmount; tsf_u16 wordAmount; };
struct tsf_hydra_phdr { tsf_char20 presetName; tsf_u16 preset, bank, presetBagNdx; tsf_u32 library, genre, morphology; };
struct tsf_hydra_pbag { tsf_u16 genNdx, modNdx; };
struct tsf_hydra_pmod { tsf_u16 modSrcOper, modDestOper; tsf_s16 modAmount; tsf_u16 modAmtSrcOper, modTransOper; };
struct tsf_hydra_pgen { tsf_u16 genOper; union tsf_hydra_genamount genAmount; };
struct tsf_hydra_inst { tsf_char20 instName; tsf_u16 instBagNdx; };
struct tsf_hydra_ibag { tsf_u16 instGenNdx, instModNdx; };
struct tsf_hydra_imod { tsf_u16 modSrcOper, modDestOper; tsf_s16 modAmount; tsf_u16 modAmtSrcOper, modTransOper; };
struct tsf_hydra_igen { tsf_u16 genOper; union tsf_hydra_genamount genAmount; };
struct tsf_hydra_shdr { tsf_char20 sampleName; tsf_u32 start, end, startLoop, endLoop, sampleRate; tsf_u8 originalPitch; tsf_s8 pitchCorrection; tsf_u16 sampleLink, sampleType; };

struct tsf_riffchunk { tsf_fourcc id; tsf_u32 size; };
struct tsf_envelope { float delay, attack, hold, decay, sustain, release, keynumToHold, keynumToDecay; };
struct tsf_voice_envelope { float level, slope; int samplesUntilNextSegment; short segment, midiVelocity; struct tsf_envelope parameters; TSF_BOOL segmentIsExponential, isAmpEnv; };
struct tsf_voice_lowpass { double QInv, a0, a1, b1, b2, z1, z2; TSF_BOOL active; };
struct tsf_voice_lfo { int samplesUntil; float level, delta; };

struct tsf_modoper {
	unsigned char index:7;
	unsigned char cc:1; // cc=0=index is general control; cc=1=index is midi control
	unsigned char d:1; // d=0=positive (0 -> 127); d=1=negative (127 -> 0)
	unsigned char p:1; // p=0=unipolar(0 -> 1); p=1=bipolar(-1 -> +1)
	unsigned char type:6; // 0=linear, 1=concave, 2=convex, 4=switch
};

struct tsf_modulator
{
	union {
		unsigned int modSrcOper;
		struct tsf_modoper modSrcOperDetails;
	};
	unsigned int modDestOper;
	int modAmount;
	union {
		unsigned int modAmtSrcOper;
		struct tsf_modoper modAmtSrcOperDetails;
	};
	unsigned int modTransOper;
};

struct tsf_region
{
	int loop_mode;
	unsigned int sample_rate;
	unsigned char lokey, hikey, lovel, hivel;
	unsigned int group, offset, end, loop_start, loop_end;
	int transpose, tune, pitch_keycenter, pitch_keytrack;
	float attenuation, pan;
	struct tsf_envelope ampenv, modenv;
	int initialFilterQ, initialFilterFc;
	int modEnvToPitch, modEnvToFilterFc, modLfoToFilterFc, modLfoToVolume;
	float delayModLFO;
	int freqModLFO, modLfoToPitch;
	float delayVibLFO;
	int freqVibLFO, vibLfoToPitch;
	float reverbSend, chorusSend;
	int sampleID, instrumentID;
	int modulatorNum;
	struct tsf_modulator* modulators;
};

struct tsf_preset
{
	char presetName[21];
	tsf_u16 preset, bank;
	struct tsf_region* regions;
	int regionNum;
};

struct tsf_sample
{
    char sampleName[21];
    tsf_u32 start, end, startLoop, endLoop, sampleRate;
    tsf_u8 originalPitch;
    tsf_s8 pitchCorrection;
    tsf_u16 sampleLink, sampleType;
};

struct tsf_voice
{
	int playingPreset, playingKey, playingChannel, heldSustain;
	struct tsf_region* region;
	double pitchInputTimecents, pitchOutputFactor;
	double sourceSamplePosition;
	float  noteGainDB, panFactorLeft, panFactorRight;
	unsigned int playIndex, loopStart, loopEnd;
	struct tsf_voice_envelope ampenv, modenv;
	struct tsf_voice_lowpass lowpass;
	struct tsf_voice_lfo modlfo, viblfo;
};

struct tsf_channel
{
	unsigned short presetIndex, bank, pitchWheel, midiPan, midiVolume, midiExpression, midiRPN, midiData, sustain;
	float panOffset, gainDB, pitchRange, tuning;
};

struct tsf_channels
{
	void (*setupVoice)(tsf* f, struct tsf_voice* voice);
	int channelNum, activeChannel;
	struct tsf_channel channels[1];
};

double tsf_timecents2Secsd(double timecents);
float tsf_timecents2Secsf(float timecents);
float tsf_cents2Hertz(float cents);
float tsf_decibelsToGain(float db);
float tsf_gainToDecibels(float gain);

#ifdef __cplusplus
}
#endif

#endif