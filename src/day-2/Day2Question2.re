/*
  --- Part Two ---

 Confident that your list of box IDs is complete,
 you're ready to find the boxes full of prototype fabric.

 The boxes will have IDs which differ by exactly one character
 at the same position in both strings.

 For example, given the following box IDs:

 abcde
 fghij
 klmno
 pqrst
 fguij
 axcye
 wvxyz

 The IDs abcde and axcye are close, but they differ by two characters(the second and fourth).
 However, the IDs fghij and fguij differ by exactly one character, the third (h and u).
 Those must be the correct boxes.

 What letters are common between the two correct box IDs? (In the example above,
 this is found by removing the differing character from either ID, producing fgij.)

  */
type foo = {
  boxIDs: array(string),
  differenceAt: int,
};

let rec listMismatches = (boxID, otherBoxID, mismatches, index) => {
  let boxIDLength = String.length(boxID);
  index == boxIDLength ?
    mismatches :
    listMismatches(
      boxID,
      otherBoxID,
      Js.String.charAt(index, boxID) == Js.String.charAt(index, otherBoxID) ?
        mismatches : [index, ...mismatches],
      index + 1,
    );
};

let rec offByOnePair = (remainingBoxIDs, boxID) => {
  switch (remainingBoxIDs) {
  | [] => None
  | [nextBoxID, ...rest] =>
    boxID == nextBoxID ?
      offByOnePair(rest, boxID) :
      {
        let mismatches = listMismatches(boxID, nextBoxID, [], 0);
        switch (mismatches) {
        | [index] =>
          Some({boxIDs: [|boxID, nextBoxID|], differenceAt: index})
        | _ => offByOnePair(rest, boxID)
        };
      }
  };
};

let rec findOffByOneBoxIDs = boxIDs =>
  switch (boxIDs) {
  | [boxID, ...rest] =>
    switch (offByOnePair(rest, boxID)) {
    | Some(result) => Some(result)
    | None => findOffByOneBoxIDs(rest)
    }
  | _ => None
  };

let logResult = result =>
  switch (result) {
  | Some(res) =>
    let boxID = res.boxIDs[0];
    Js.log(
      Js.String.substring(~from=0, ~to_=res.differenceAt, boxID)
      ++ Js.String.substringToEnd(~from=res.differenceAt + 1, boxID),
    );
  | None => Js.log("Unable to find the answer")
  };

let groups =
  "day2.txt"
  ->Util.readLinesOfFile
  ->Belt.List.fromArray
  ->findOffByOneBoxIDs
  ->logResult;