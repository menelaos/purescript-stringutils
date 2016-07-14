"use strict";

exports._fromCodePoint = function (Just) {
  return function (Nothing) {
    return function (i) {
      return i >= 0 && i <= 0x10FFFF ? Just(String.fromCodePoint(i)) : Nothing;
    };
  };
};

exports.toCodePoint = function (c) {
  return c.codePointAt(0);
};
