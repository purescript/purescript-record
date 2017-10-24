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

exports.runSTRecord = function(rec) {
  return function() {
    return copyRecord(rec());
  };
};

exports.freezeSTRecord = function(rec) {
  return function() {
    return copyRecord(rec);
  };
};

exports.thawSTRecord = function(rec) {
  return function() {
    return copyRecord(rec);
  };
};

exports.unsafePeekSTRecord = function(l) {
  return function(rec) {
    return function() {
      return rec[l];
    };
  };
};

exports.unsafePokeSTRecord = function(l) {
  return function(a) {
    return function(rec) {
      return function() {
        rec[l] = a;
      };
    };
  };
};
