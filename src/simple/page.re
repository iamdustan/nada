module Page = {
  include Synth;
  include ReactRe.Component.Stateful;
  type props = {message: string};
  type state = {
    color: string,
    x: int,
    y: int,
  };

  let max a b => a > b ? a : b;
  let screenHeight = 700;

  let getInitialState _ /* props */ => {
    color: "yellow",
    x: 0,
    y: 1,
  };

  let name = "Page";
  let handleMouseMove {state} event => {
    let x = ReactEventRe.Mouse.pageX event;
    let y = ReactEventRe.Mouse.pageY event;
    Some {
      ...state,
      x,
      y: max y screenHeight,
      color: y > (screenHeight / 2) ? "blue" : "yellow"
    };
  };


  let render {props, updater, state} =>
    <div
      onMouseMove=(updater handleMouseMove)
      style=(ReactDOMRe.Style.make
        position::"absolute"
        top::"0"
        bottom::"0"
        left::"0"
        right::"0"
        background:: state.color
        ()
      )
    > (ReactRe.stringToElement props.message) </div>;
};

include ReactRe.CreateComponent Page;

let createElement ::message => wrapProps {message: message};
