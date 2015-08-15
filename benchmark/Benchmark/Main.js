'use strict';

// module Benchmark.Main

function zeroArray(size) {
  return function(){
    var arr = [];
    for (var i = 0; i < size; i++) {
      arr[i] = 0;
    }
    return arr;
  };
}

exports.zeroArray = zeroArray;
