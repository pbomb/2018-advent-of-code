let readLinesOfFile = filename => {
  let dirname =
    switch ([%bs.node __dirname]) {
    | Some(d) => d
    | None => ""
    };

  Js.String.split(
    "\n",
    Node.Fs.readFileAsUtf8Sync(
      [|dirname, "inputs", filename|]->Node.Path.join,
    ),
  );
};