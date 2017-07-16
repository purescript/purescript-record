"use strict";

exports.unsafeGet = function(label, rec) {
  return rec[label];
}

exports.unsafeSet = function(label, value, rec) {
  var copy = {};
  for (var key in rec) {
      if ({}.hasOwnProperty.call(v, key)) {
          copy[key] = rec[key];
      };
  };
  copy[label] = value;
  return copy;
}
