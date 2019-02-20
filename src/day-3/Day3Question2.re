/*
 --- Part Two ---
 Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single
 square inch of fabric with any other claim. If you can somehow draw attention to it,
 maybe the Elves will be able to make Santa's suit after all!

 For example, in the claims above, only claim 3 is intact after all claims are made.

 What is the ID of the only claim that doesn't overlap?
 */
open Day3Util;

type coords = (int, int);

type region = {
  id: int,
  xStart: int,
  yStart: int,
  xEnd: int,
  yEnd: int,
  edges: Belt.List.t(coords),
};

/**
 * Compute a list of integers starting with `start`,
 * up to and including `end_`.
 */
let rec range = (start: int, end_: int) =>
  if (start > end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let toEdges =
    ({id, fromEdges: (xStart, yStart), dimensions: (xSize, ySize)}) => {
  let xEnd = xStart + xSize - 1;
  let yEnd = yStart + ySize - 1;
  {
    id,
    xStart,
    yStart,
    xEnd,
    yEnd,
    edges:
      Belt.List.concatMany([|
        range(xStart, xEnd)->Belt.List.map(x => (x, yStart)),
        range(xStart, xEnd)->Belt.List.map(x => (x, yEnd - 1)),
        range(yStart, yEnd)->Belt.List.map(y => (xStart, y)),
        range(yStart, yEnd)->Belt.List.map(y => (xEnd - 1, y)),
      |]),
  };
};

let withinRegion = ({xStart, yStart, xEnd, yEnd}, (x, y)) =>
  x >= xStart && x <= xEnd && y >= yStart && y <= yEnd;

let regionsOverlap = (region1, region2) =>
  region1.edges->Belt.List.some(withinRegion(region2));

let markOverlaps = (remainingRegions, overlaps, region) =>
  remainingRegions->Belt.List.forEach(remainingRegion => {
    // optimization by checking Set before re-calculating
    let bothAlreadyMarked =
      [remainingRegion, region]
      ->Belt.List.every(r => overlaps->Belt.MutableSet.Int.has(r.id));
    if (remainingRegion != region
        && !bothAlreadyMarked
        && regionsOverlap(remainingRegion, region)) {
      [remainingRegion, region]
      ->Belt.List.forEach(r => overlaps->Belt.MutableSet.Int.add(r.id));
    };
  });

let regionsWithoutOverlap = regions => {
  let overlaps = Belt.MutableSet.Int.make();
  regions->Belt.List.forEach(markOverlaps(regions, overlaps));
  regions->Belt.List.keep(r => !overlaps->Belt.MutableSet.Int.has(r.id));
};

let logRegion = regions =>
  switch (regions) {
  | [region, ...rest] =>
    rest->Belt.List.length == 0 ?
      region.id->Js.log :
      regions->Belt.List.toArray->Belt.Array.map(r => r.id)->Js.log
  | [] => Js.log("Could not find region without overlaps")
  };

readClaims()->Belt.List.map(toEdges)->regionsWithoutOverlap->logRegion;