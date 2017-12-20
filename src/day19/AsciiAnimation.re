type t;

[@bs.new] [@bs.module] external make : unit => t = "cli-frames";

[@bs.send] external load : (t, array(string)) => t = "";

[@bs.send] external start : t => t = "";

module Dev = {
  let printFrame: string => unit = [%bs.raw
    {|
    function (frame) {
      var readline = require('readline');
      readline.clearLine(process.stdout);
      readline.cursorTo(process.stdout, 0);
      process.stdout.write(frame);
    }
  |}
  ];
  let animate = (frames) => Array.iter((frame) => printFrame(frame), frames);
};