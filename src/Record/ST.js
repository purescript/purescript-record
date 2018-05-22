"use strict";

function copyRecord(rec) {
  var copy = {};
  for (var key in rec) {
    if ({}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  return copy;
}

exports.freeze = function(rec) {
  return function() {
    return copyRecord(rec);
  };
};

exports.thaw = function(rec) {
  return function() {
    return copyRecord(rec);
  };
};

exports.unsafePeek = function(l) {
  return function(rec) {
    return function() {
      return rec[l];
    };
  };
};

exports.unsafePoke = function(l) {
  return function(a) {
    return function(rec) {
      return function() {
        rec[l] = a;
      };
    };
  };
};

exports.unsafeModify = function(l) {
  return function(f) {
    return function(rec) {
      return function() {
        rec[l] = f(rec[l]);
      };
    };
  };
};
