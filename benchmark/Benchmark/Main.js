'use strict';

// module Benchmark.Main

function randomArray(size) {
  return function(){
    var arr = [];
    for (var i = 0; i < size; i++) {
      arr[i] = Math.random();
    }
    return arr;
  };
}

function zeroArray(size) {
  return function(){
    var arr = [];
    for (var i = 0; i < size; i++) {
      arr[i] = 0;
    }
    return arr;
  };
}

exports.randomArray = randomArray;

exports.zeroArray = zeroArray;
