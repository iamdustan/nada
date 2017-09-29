module Document = {
  type element;
  type window;
  let window: window = [%bs.raw "window"];
  /* external setGlDebug : window => GlT.context => unit = "debugContext" [@@bs.set]; */
  external getElementById : string => element = "document.getElementById" [@@bs.val];
  external getContext : element => string => 'context = "getContext" [@@bs.send];
  external getWidth : element => int = "width" [@@bs.get];
  external getHeight : element => int = "height" [@@bs.get];
  external requestAnimationFrame : (unit => unit) => unit = "window.requestAnimationFrame" [@@bs.val];
  external now : unit => float = "Date.now" [@@bs.val];
  external addEventListener : 'window => string => ('eventT => unit) => unit = "addEventListener" [@@bs.send];
};

external getButton : 'eventT => int = "button" [@@bs.get];

external getClientX : 'eventT => int = "clientX" [@@bs.get];

external getClientY : 'eventT => int = "clientY" [@@bs.get];

external getWhich : 'eventT => int = "which" [@@bs.get];

external getBoundingClientRect : 'canvas => 'leftAndTop = "getBoundingClientRect" [@@bs.send];

external getTop : 'a => int = "top" [@@bs.get];

external getLeft : 'a => int = "left" [@@bs.get];

external getWidth : 'canvas => int = "width" [@@bs.get];

external getHeight : 'canvas => int = "height" [@@bs.get];

external setWidth : 'canvas => int => unit = "width" [@@bs.set];

external setHeight : 'canvas => int => unit = "height" [@@bs.set];

external createElement : string => 'canvas = "document.createElement" [@@bs.val];

let createCanvas () => createElement "canvas";

external addToBody : 'canvas => unit = "document.body.appendChild" [@@bs.val];

external getContext : 'canvas => string => 'options => 'context = "getContext" [@@bs.send];

type styleT;

external getStyle : 'canvas => styleT = "style" [@@bs.get];

external setBackgroundColor : styleT => string => unit = "backgroundColor" [@@bs.set];

type httpRequestT;

external makeXMLHttpRequest : unit => httpRequestT = "XMLHttpRequest" [@@bs.new];

external openFile : httpRequestT =>
                    kind::string =>
                    filename::string =>
                    whatIsThis::Js.boolean =>
                    unit = "open" [@@bs.send];

external onreadystatechange : httpRequestT => (unit => unit) => unit = "onreadystatechange" [@@bs.set];

external getReadyState : httpRequestT => int = "readyState" [@@bs.get];

external getStatus : httpRequestT => int = "status" [@@bs.get];

external getResponseText : httpRequestT => string = "responseText" [@@bs.get];

external sendRequest : httpRequestT => Js.null 'a => unit = "send" [@@bs.send];

module Gl: ReasonglInterface.Gl.t = {
  let target = "web";
  type contextT;
  module type FileT = {type t; let readFile: filename::string => cb::(string => unit) => unit;};
  module File = {
    type t;
    let readFile ::filename ::cb => {
      let rawFile = makeXMLHttpRequest ();
      openFile rawFile kind::"GET" ::filename whatIsThis::Js.false_;
      onreadystatechange
        rawFile
        (
          fun () =>
            if (
              getReadyState rawFile === 4 && (getStatus rawFile === 200 || getStatus rawFile === 0)
            ) {
              cb (getResponseText rawFile)
            }
        );
      sendRequest rawFile Js.null
    };
  };
  module type WindowT = {
    type t;
    let getWidth: t => int;
    let getHeight: t => int;
    let init: argv::array string => t;
    let setWindowSize: window::t => width::int => height::int => unit;
    let initDisplayMode: window::t => double_buffer::bool => unit => unit;
    let getContext: t => contextT;
  };
  module Window = {
    type t;
    let getWidth = getWidth;
    let getHeight = getHeight;
    let init argv::_ => {
      let canvas: t = createCanvas ();
      setBackgroundColor (getStyle canvas) "black";
      addToBody canvas;
      canvas
    };
    let setWindowSize window::(window: t) ::width ::height => {
      setWidth window width;
      setHeight window height
    };
    let initDisplayMode ::window double_buffer::_ () => ();
    let getContext (window: t) :contextT =>
      getContext window "webgl" {"preserveDrawingBuffer": true, "antialias": false};
  };

  module Events = Events;

  type mouseButtonEventT =
    button::Events.buttonStateT => state::Events.stateT => x::int => y::int => unit;

  /** See Gl.re for explanation. **/
  let render
      window::(canvas: Window.t)
      mouseDown::(mouseDown: option mouseButtonEventT)=?
      mouseUp::(mouseUp: option mouseButtonEventT)=?
      mouseMove::(mouseMove: option (x::int => y::int => unit))=?
      keyDown::(keyDown: option (keycode::Events.keycodeT => repeat::bool => unit))=?
      keyUp::(keyUp: option (keycode::Events.keycodeT => unit))=?
      windowResize::(windowResize: option (unit => unit))=?
      displayFunc::(displayFunc: float => unit)
      () => {
    switch mouseDown {
    | None => ()
    | Some cb =>
      Document.addEventListener
        canvas
        "mousedown"
        (
          fun e => {
            let button =
              switch (getButton e) {
              | 0 => Events.LeftButton
              | 1 => Events.MiddleButton
              | 2 => Events.RightButton
              | _ => assert false
              };
            let state = Events.MouseDown;
            let rect = getBoundingClientRect canvas;
            let x = getClientX e - getLeft rect;
            let y = getClientY e - getTop rect;
            cb ::button ::state ::x ::y
          }
        )
    };
    switch mouseUp {
    | None => ()
    | Some cb =>
      Document.addEventListener
        canvas
        "mouseup"
        (
          fun e => {
            let button =
              switch (getButton e) {
              | 0 => Events.LeftButton
              | 1 => Events.MiddleButton
              | 2 => Events.RightButton
              | _ => assert false
              };
            let state = Events.MouseUp;
            let rect = getBoundingClientRect canvas;
            let x = getClientX e - getLeft rect;
            let y = getClientY e - getTop rect;
            cb ::button ::state ::x ::y
          }
        )
    };
    switch mouseMove {
    | None => ()
    | Some cb =>
      Document.addEventListener
        canvas
        "mousemove"
        (
          fun e => {
            let rect = getBoundingClientRect canvas;
            let x = getClientX e - getLeft rect;
            let y = getClientY e - getTop rect;
            cb ::x ::y
          }
        )
    };
    let keyLastPressed = ref None;
    switch keyDown {
    | None => ()
    | Some cb =>
      Document.addEventListener
        Document.window /* This is the real window. The "window" param is the canvas. */
        "keydown"
        (
          fun e => {
            let keycode = (getWhich e);
            let repeat =
              switch !keyLastPressed {
              | None => false
              | Some k => k === keycode
              };
            keyLastPressed := Some keycode;
            cb keycode::(Events.keycodeMap keycode) ::repeat
          }
        )
    };
    switch keyUp {
    | None => ()
    | Some cb =>
      Document.addEventListener
        Document.window
        "keyup"
        (
          fun e => {
            let keycode = (getWhich e);
            keyLastPressed := None;
            cb keycode::(Events.keycodeMap keycode)
          }
        )
    };
    let rec tick prev () => {
      let now = Document.now ();
      displayFunc (now -. prev);
      Document.requestAnimationFrame (tick now)
    };
    Document.requestAnimationFrame (tick (Document.now ()))
  };
  type programT;
  type shaderT;
  external clearColor : context::contextT => r::float => g::float => b::float => a::float => unit = "clearColor" [@@bs.send];
  external createProgram : context::contextT => programT = "createProgram" [@@bs.send];
  external createShader : context::contextT => shaderType::int => shaderT = "createShader" [@@bs.send];
  external _shaderSource : context::contextT => shader::shaderT => source::string => unit = "shaderSource" [@@bs.send];
}
