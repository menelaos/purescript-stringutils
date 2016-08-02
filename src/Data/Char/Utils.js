"use strict";

function _fromCodePoint (Just) {
  return function (Nothing) {
    return function (i) {
      return i >= 0 && i <= 0x10FFFF ? Just(String.fromCodePoint(i)) : Nothing;
    };
  };
}

function toCodePoint (c) {
  return c.codePointAt(0);
}

function unsafeFromCodePoint (cp) {
  if (cp >= 0 && cp <= 0x10FFFF) {
    return String.fromCodePoint(cp);
  }
  else {
    throw new Error("Data.Char.Utils.unsafeFromCodePoint': Invalid codepoint");
  }
}

exports._fromCodePoint      = _fromCodePoint;
exports.toCodePoint         = toCodePoint;
exports.unsafeFromCodePoint = unsafeFromCodePoint;
