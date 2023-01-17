(1..8).collect {|i|
SynthDef("PlayBuf_%".format(i), {
	arg out=0, bufnum, rate=1.0, trigger=1.0, startPos=0, loop=0, doneAction=0;

	OffsetOut.ar(out,
		PlayBuf.ar(i,
			bufnum, BufRateScale.kr(bufnum) * rate,
			trigger, startPos, loop, doneAction
		)
	)
}, [\ir, \kr, \kr, \tr])
}.writeDefFile("PlayBuf", ".");

SynthDef(\default, {
	arg out=0, freq=440, amp=0.1, pan=0, gate=1;
	var z = LPF.ar(
		Mix.new(
			VarSaw.ar(freq + [0, Rand(-0.4,0.0), Rand(0.0,0.4)], 0, 0.3, 0.3)
		),
		XLine.kr(Rand(4000,5000), Rand(2500,3200), 1)
	) * Linen.kr(gate, 0.01, 0.7, 0.3, 2);
	OffsetOut.ar(out, Pan2.ar(z, pan, amp));
}, [\ir]).writeDefFile(".");
