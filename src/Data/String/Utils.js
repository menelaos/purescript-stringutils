"use strict";

exports._codePointAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.codePointAt(i)) : nothing;
      };
    };
  };
};

exports.endsWith = function (searchString) {
  return function (s) {
    return s.endsWith(searchString);
  };
};

exports.escapeRegex = function (str) {
  return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
};

exports.startsWith = function (searchString) {
  return function (s) {
    return s.startsWith(searchString);
  };
};

exports.startsWithP = function (searchString) {
  return function (position) {
    return function (s) {
      return s.startsWith(searchString, position);
    };
  };
};
