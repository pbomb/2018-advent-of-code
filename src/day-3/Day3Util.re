type claim = {
  id: int,
  fromEdges: (int, int),
  dimensions: (int, int),
};

let parseClaim = claimLine => {
  let claimParts = claimLine |> Js.String.split(" ");
  let edgeParts =
    (
      claimParts[2]->Js.String.slice(~from=0, ~to_=-1) |> Js.String.split(",")
    )
    ->Belt.Array.map(int_of_string);
  let dimensionParts =
    (claimParts[3] |> Js.String.split("x"))->Belt.Array.map(int_of_string);
  {
    id: claimParts[0]->Js.String.substringToEnd(~from=1)->int_of_string,
    fromEdges: (edgeParts[0], edgeParts[1]),
    dimensions: (dimensionParts[0], dimensionParts[1]),
  };
};

let readClaims = () =>
  "day3.txt"
  ->Util.readLinesOfFile
  ->Belt.List.fromArray
  ->Belt.List.map(parseClaim);