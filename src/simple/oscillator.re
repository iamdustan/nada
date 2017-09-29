Js.log "oscillator loaded";

open Containers;
type destination;
type pitch = C | Cs | D | Eb | E | F | Fs | G | Gs | A | Bb | B;
module Pitches = Map.Make({ type t = pitch; let compare = compare; });

type waveType = Sine | Square;

let pitches = Pitches.fromList
  [
    (C, 16.35),
    (Cs, 17.32),
    (D, 18.35),
    (Eb, 19.45),
    (E, 20.60),
    (F, 21.83),
    (Fs, 23.12),
    (G, 24.50),
    (Gs, 25.96),
    (A, 27.50),
    (Bb, 29.14),
    (B, 30.87),
  ];

let rec pow b e => {
  e == 0.0 ? 1.0 : b *. pow b (e -. 1.0);
};

let getPitch pitch octave => {
  (pow 2.0 octave) *. Pitches.find pitch pitches;
};

type oscillator = Js.t {
  .
  frequency: Js.t {
    .
    value [@bs.set] [@bs.get]: float
  },
  connect [@bs.meth] : destination => unit,
  _type [@bs.set] [@bs.get] : string,
  start [@bs.meth] : float => unit,
  stop [@bs.meth] : float => unit
};

type audioContext = Js.t {
  .
  destination: destination,
  createOscillator [@bs.meth] : unit => oscillator
};

external audioContext : unit => audioContext = "window.AudioContext" [@@bs.new];
external createOscillator : audioContext => unit => oscillator = "" [@@bs.send];

let c = audioContext ();

let play note => {
  let o: oscillator = c##createOscillator ();
  o##_type #= "square";
  o##frequency##value #= (getPitch note 4.0);
  o##connect (c##destination);
  o##start (0.25);
  o;
};

let o = play B;

Js_global.setTimeout (fun _ => {o##frequency##value #= (getPitch C 4.0)}) 1000;

Js_global.setTimeout (fun _ => {o##stop 0.0}) 10000;
