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
}

function _codePointAtP (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.codePointAt(i)) : nothing;
      };
    };
  };
}

function endsWith (searchString) {
  return function (s) {
    return s.endsWith(searchString);
  };
}

function endsWithP (searchString) {
  return function (position) {
    return function (s) {
      return s.endsWith(searchString, position);
    };
  };
}

function escapeRegex (str) {
  return str.replace(/[.*+?^${}()|[\]\-\\]/g, "\\$&");
}

function fromCharArray (array) {
  return array.join("");
}

function includes (searchString) {
  return function (str) {
    return str.includes(searchString);
  };
}

function includesP (needle) {
  return function (position) {
    return function (haystack) {
      // For negative `position` values, we search from the beginning of the
      // string. This is in accordance with the native
      // `String.prototype.include` function.
      var pos = Math.max(0, position);

      // Converting to arrays takes care of any surrogate code points
      var needleA    = Array.from(needle);
      var haystackA  = Array.from(haystack).slice(pos);
      var needleALen = needleA.length;

      var maxIndex = haystackA.length + 1 - needleALen;
      var found    = false;
      var i;

      // Naive implementation, at some point we should check whether Boyer-Moore
      // or Knuth-Morris-Pratt are worthwhile
      for (i = 0; i < maxIndex; i++) {
        if (needleA.every(function (e, j) { return e === haystackA[i+j]; })) {
          found = true;
          break;
        }
      }

      return found;
    };
  };
}

function length (str) {
  return Array.from(str).length;
}

function lines (str) {
  // See http://www.unicode.org/reports/tr18/#RL1.6
  return str.split(/\r\n|[\n\v\f\r\u0085\u2028\u2029]/);
}

function normalize (str) {
  return str.normalize();
}

function _normalizeP (normalizationForm) {
  return function (str) {
    return str.normalize(normalizationForm);
  };
}

function _repeat (just) {
  return function (nothing) {
    return function (n) {
      return function (str) {
        var result;

        try {
          result = just(str.repeat(n));
        }
        catch (error) {
          result = nothing;
        }

        return result;
      };
    };
  };
}

function startsWith (searchString) {
  return function (s) {
    return s.startsWith(searchString);
  };
}

function startsWithP (searchString) {
  return function (position) {
    return function (s) {
      return s.startsWith(searchString, position);
    };
  };
}

function stripChars (chars) {
  return function (s) {
    return s.replace(RegExp("[" + escapeRegex(chars) + "]", "g"), "");
  };
}

function stripDiacritics (str) {
  return str.normalize("NFD").replace(/[\u0300-\u036f]/g, "");
}

function toCharArray (str) {
  return Array.from(str);
}

function unsafeCodePointAt (i) {
  return function (s) {
    var codePointArray = Array.from(s);
    var isWithinRange = i >= 0 && i < codePointArray.length;

    if (isWithinRange) {
      return codePointArray[i].codePointAt(0);
    }
    else {
      throw new Error("Data.String.Utils.unsafeCodePointAt: Invalid index");
    }
  };
}

function unsafeCodePointAtP (i) {
  return function (s) {
    if (i >= 0 && i < s.length) {
      return s.codePointAt(i);
    }
    else {
      throw new Error("Data.String.Utils.unsafeCodePointAt': Invalid index");
    }
  };
}

function unsafeRepeat (n) {
  return function (str) {
    try {
      return str.repeat(n);
    }
    catch (error) {
      throw new Error("Data.String.Utils.unsafeRepeat: Invalid count");
    }
  };
}

function words (s) {
  return s.split(/\s+/);
}

exports._codePointAt       = _codePointAt;
exports._codePointAtP      = _codePointAtP;
exports.endsWith           = endsWith;
exports.endsWithP          = endsWithP;
exports.escapeRegex        = escapeRegex;
exports.fromCharArray      = fromCharArray;
exports.includes           = includes;
exports.includesP          = includesP;
exports.length             = length;
exports.lines              = lines;
exports.normalize          = normalize;
exports._normalizeP        = _normalizeP;
exports._repeat            = _repeat;
exports.startsWith         = startsWith;
exports.startsWithP        = startsWithP;
exports.stripChars         = stripChars;
exports.stripDiacritics    = stripDiacritics;
exports.toCharArray        = toCharArray;
exports.unsafeCodePointAt  = unsafeCodePointAt;
exports.unsafeCodePointAtP = unsafeCodePointAtP;
exports.unsafeRepeat       = unsafeRepeat;
exports.words              = words;
