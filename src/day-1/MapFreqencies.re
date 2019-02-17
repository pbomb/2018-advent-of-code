let mapFrequencies = () => {
  let dirname =
    switch ([%bs.node __dirname]) {
    | Some(d) => d
    | None => ""
    };
  Js.String.split(
    "\n",
    Node.Fs.readFileAsUtf8Sync([|dirname, "input.txt"|]->Node.Path.join),
  )
  ->Belt.List.fromArray
  ->Belt.List.map(Js.Float.fromString);
};
