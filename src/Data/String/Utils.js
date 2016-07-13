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

exports.endsWithP = function (searchString) {
  return function (position) {
    return function (s) {
      return s.endsWith(searchString, position);
    };
  };
};

exports.escapeRegex = function (str) {
  return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
};

exports.includes = function (searchString) {
  return function (str) {
    return str.includes(searchString);
  };
};

exports.length = function (str) {
  return Array.from(str).length;
};

exports.normalize = function (str) {
  return str.normalize();
};

exports._normalizeP = function (normalizationForm) {
  return function (str) {
    return str.normalize(normalizationForm);
  };
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

exports.stripChars = function (chars) {
  return function (s) {
    return s.replace(RegExp("[" + chars + "]", "g"), "");
  };
};

exports.toCharArray = function (str) {
  return Array.from(str);
};
