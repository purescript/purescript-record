"use strict";

exports.copyRecord = function(rec) {
  var copy = {};
  for (var key in rec) {
    if ({}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  return copy;
};

exports.unsafeInsert = function(l) {
  return function(a) {
    return function(rec) {
      rec[l] = a;
      return rec;
    };
  };
};

exports.unsafeDelete = function(l) {
  return function(rec) {
    delete rec[l];
    return rec;
  };
};

exports.unsafeMerge = function(r1) {
  return function(r2) {
    var copy = {};
    for (var key in r2) {
      if ({}.hasOwnProperty.call(r2, key)) {
        copy[key] = r2[key];
      }
    }
    for (var key in r1) {
      if ({}.hasOwnProperty.call(r1, key)) {
        copy[key] = r1[key];
      }
    }
    return copy;
  };
};
