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

exports.unsafeMergeFn = function(r1, r2) {
  var r = {};
  for (var k1 in r2) {
    if ({}.hasOwnProperty.call(r2, k1)) {
      r[k1] = r2[k1];
    }
  }
  for (var k2 in r1) {
    if ({}.hasOwnProperty.call(r1, k2)) {
      r[k2] = r1[k2];
    }
  }
  return r;
}
