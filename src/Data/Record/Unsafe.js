"use strict";

exports.unsafeGetFn = function(label, rec) {
  return rec[label];
};

exports.unsafeSetFn = function(label, value, rec) {
  var copy = {};
  for (var key in rec) {
    if ({}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  copy[label] = value;
  return copy;
};

exports.unsafeDeleteFn = function(label, rec) {
  var copy = {};
  for (var key in rec) {
    if (key !== label && {}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  return copy;
};

exports.unsafeHasFn = function(label, rec) {
  return {}.hasOwnProperty.call(rec, label);
};
