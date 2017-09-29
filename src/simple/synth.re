open Containers;
open AudioContext;

external pow : float => float => float = "Math.pow" [@@bs.val];
external requestAnimationFrame : (unit => unit) => unit = "window.requestAnimationFrame" [@@bs.val];

module Synth = {
  type octave = float;
  type duration = float;
  type pitch = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B;
  type note = Note pitch octave duration | Rest duration;
  type noteArray = Js_typed_array.Float32Array.t;

  module Notes = Map.Make({ type t = pitch; let compare = compare; });

  let notes = Notes.fromList [
    (C, 16.35),
    (Cs, 17.32),
    (D, 18.35),
    (Ds, 19.45),
    (E, 20.60),
    (F, 21.83),
    (Fs, 23.12),
    (G, 24.50),
    (Gs, 25.96),
    (A, 27.50),
    (As, 29.14),
    (B, 30.87),
  ];

  let makeNote : AudioContext.t => note => AudioBuffer.t  = fun ctx note => {
    switch note {
    | Note pitch octave duration => {
      let length = truncate @@ duration *. ctx##sampleRate;
      let c = ctx##createBuffer 2 (length) ctx##sampleRate;
      let freq = (pow 2.0 octave) *. Notes.find pitch notes;
      let noteDivisor = (ctx##sampleRate /. freq);
      let b = Array.make length 1.0
        |> Array.mapi (fun i _ => Js_math.sin (3.1415 *. 2.0 *. (float i) /. noteDivisor))
        |> Js_typed_array.Float32Array.make;

      c##copyToChannel b 0 0;
      c##copyToChannel b 1 0;
      c;
    }
    | Rest duration => {
      let length = truncate @@ duration *. ctx##sampleRate;
      let c = ctx##createBuffer 2 (length) ctx##sampleRate;
      let b = Array.make length 0.0
        |> Js_typed_array.Float32Array.make;
      c##copyToChannel b 0 0;
      c##copyToChannel b 1 0;
      c;
    }
    }
  };

  let stop = fun source time => {
    AudioBufferSourceNode.stop source time;
  };

  let play : AudioContext.t => AudioBuffer.t => float => AudioBufferSourceNode.t = fun audioCtx c time => {
    let source = audioCtx##createBufferSource ();

    AudioBufferSourceNode.setBuffer source c;
    AudioBufferSourceNode.connect source audioCtx##destination;
    AudioBufferSourceNode.start source time;
    source;
  };
};

let audioCtx = AudioContext.make ();
if (audioCtx##state === AudioContextState.suspended) {
  Js.log("hmm");
};

let start = audioCtx##currentTime +. 0.05;
let spn = 60.0 /. 80.0;

let rec schedule (currentEnd, playing) (duration, source) list => {
  let currentTime = audioCtx##currentTime -. start;

  if (currentEnd < currentTime +. 0.25) {
    Synth.stop playing (currentEnd);
    let nowPlaying = Synth.play audioCtx source currentEnd;

    if (List.length list > 0) {
      let [(n1, n2), ...rest] = list;
      requestAnimationFrame (fun _ => schedule (spn *. duration +. currentTime, nowPlaying) (n1, n2) rest);
    } else {
      Synth.stop nowPlaying (currentEnd +. duration *. spn);
    };
  } else {
    requestAnimationFrame (fun _ => schedule (currentEnd, playing) (duration, source) list);
  };
};

let main song => {
  switch song {
  | [] => ()
  | [(d, s)] => {
    let x = Synth.play audioCtx s start;
  }
  | [(d, s), (d', s'), ...rest] => {
    let x = Synth.play audioCtx s start;
    schedule (start +. d *. spn, x) (d', s') rest;
  }
  };
};

let f = fun x => {
  switch x {
  | Synth.Note n o d => (d, Synth.makeNote audioCtx (Note n o d))
  | Synth.Rest d => (d, Synth.makeNote audioCtx (Rest d))
  };
};

main @@ List.map f [
  Synth.Note E 4.0 1.0,
  Synth.Note E 4.0 1.0,
  Synth.Note F 4.0 1.0,
  Synth.Note G 4.0 1.0,
  Synth.Note G 4.0 1.0,
  Synth.Note F 4.0 1.0,
  Synth.Note E 4.0 1.0,
  Synth.Note D 4.0 1.0,
  Synth.Note C 4.0 1.0,
  Synth.Note C 4.0 1.0,
  Synth.Note D 4.0 1.0,
  Synth.Note E 4.0 1.0,
  Synth.Note E 4.0 1.75,
  Synth.Note D 4.0 0.25,
  Synth.Note D 4.0 1.0,
];

