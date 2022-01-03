"use strict";

export function codePointAtImpl (just, nothing, i, s) {
  var codePointArray = Array.from(s);
  var isWithinRange  = i >= 0 && i < codePointArray.length;

  return isWithinRange ? just(codePointArray[i].codePointAt(0)) : nothing;
}

export function codePointAtPrimeImpl (just, nothing, i, s) {
  return i >= 0 && i < s.length ? just(s.codePointAt(i)) : nothing;
}

export function endsWithImpl (searchString, s) {
  return s.endsWith(searchString);
}

export function endsWithPrimeImpl (searchString, position, s) {
  return s.endsWith(searchString, position);
}

function escapeRegexImpl (str) {
  return str.replace(/[.*+?^${}()|[\]\-\\]/g, "\\$&");
}

export function fromCharArrayImpl (array) {
  return array.join("");
}

export function includesImpl (searchString, str) {
  return str.includes(searchString);
}

export function includesPrimeImpl (needle, position, haystack) {
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
}

export function lengthImpl (str) {
  return Array.from(str).length;
}

export function linesImpl (str) {
  // See http://www.unicode.org/reports/tr18/#RL1.6
  return str.split(/\r\n|[\n\v\f\r\u0085\u2028\u2029]/);
}

export function normalizeImpl (str) {
  return str.normalize();
}

export function normalizePrimeImpl (normalizationForm, str) {
  return str.normalize(normalizationForm);
}

export function padEndPrimeImpl (targetLength, str) {
  return str.padEnd(targetLength);
}

export function padStartPrimeImpl (targetLength, str) {
  return str.padStart(targetLength);
}

export function repeatImpl (just, nothing, n, str) {
  var result;

  try {
    result = just(str.repeat(n));
  }
  catch (error) {
    result = nothing;
  }

  return result;
}

export function startsWithImpl (searchString, s) {
  return s.startsWith(searchString);
}

export function startsWithPrimeImpl (searchString, position, s) {
  return s.startsWith(searchString, position);
}

export function stripCharsImpl (chars, s) {
  return s.replace(RegExp("[" + escapeRegexImpl(chars) + "]", "g"), "");
}

export function stripDiacriticsImpl (str) {
  return str.normalize("NFD").replace(/[\u0300-\u036f]/g, "");
}

export function toCharArrayImpl (str) {
  return Array.from(str);
}

export function unsafeCodePointAtImpl (i, s) {
  var codePointArray = Array.from(s);
  var isWithinRange = i >= 0 && i < codePointArray.length;

  if (isWithinRange) {
    return codePointArray[i].codePointAt(0);
  }
  else {
    throw new Error("Data.String.Utils.unsafeCodePointAt: Invalid index");
  }
}

export function unsafeCodePointAtPrimeImpl (i, s) {
  if (i >= 0 && i < s.length) {
    return s.codePointAt(i);
  }
  else {
    throw new Error("Data.String.Utils.unsafeCodePointAt': Invalid index");
  }
}

export function unsafeRepeatImpl (n, str) {
  try {
    return str.repeat(n);
  }
  catch (error) {
    throw new Error("Data.String.Utils.unsafeRepeat: Invalid count");
  }
}

export function wordsImpl (s) {
  // Split at every Unicode whitespace character (25 as of Unicode 12.1)
  return s.split(/[\u000a-\u000d\u0085\u2028\u2029\u0009\u0020\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000]+/);
}
