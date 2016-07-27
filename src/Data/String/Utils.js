"use strict";

function _codePointAt (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        var codePointArray = Array.from(s);
        var isWithinRange  = i >= 0 && i < codePointArray.length;

        return isWithinRange ? just(codePointArray[i].codePointAt(0)) : nothing;
      };
    };
  };
};

function _codePointAtP (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.codePointAt(i)) : nothing;
      };
    };
  };
};

function endsWith (searchString) {
  return function (s) {
    return s.endsWith(searchString);
  };
};

function endsWithP (searchString) {
  return function (position) {
    return function (s) {
      return s.endsWith(searchString, position);
    };
  };
};

function escapeRegex (str) {
  return str.replace(/[.*+?^${}()|[\]\-\\]/g, "\\$&");
};

function includes (searchString) {
  return function (str) {
    return str.includes(searchString);
  };
};

function length (str) {
  return Array.from(str).length;
};

function normalize (str) {
  return str.normalize();
};

function _normalizeP (normalizationForm) {
  return function (str) {
    return str.normalize(normalizationForm);
  };
};

function startsWith (searchString) {
  return function (s) {
    return s.startsWith(searchString);
  };
};

function startsWithP (searchString) {
  return function (position) {
    return function (s) {
      return s.startsWith(searchString, position);
    };
  };
};

function stripChars (chars) {
  return function (s) {
    return s.replace(RegExp("[" + chars + "]", "g"), "");
  };
};

function toCharArray (str) {
  return Array.from(str);
};

exports._codePointAt  = _codePointAt;
exports._codePointAtP = _codePointAtP;
exports.endsWith      = endsWith;
exports.endsWithP     = endsWithP;
exports.escapeRegex   = escapeRegex;
exports.includes      = includes;
exports.length        = length;
exports.normalize     = normalize;
exports._normalizeP   = _normalizeP;
exports.startsWith    = startsWith;
exports.startsWithP   = startsWithP;
exports.stripChars    = stripChars;
exports.toCharArray   = toCharArray;
