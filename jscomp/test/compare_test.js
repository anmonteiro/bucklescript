'use strict';


function compare(x, y) {
  switch (x) {
    case /* A */0 :
        return y === /* A */0;
    case /* B */1 :
        return y === /* B */1;
    case /* C */2 :
        return y === /* C */2;
    
  }
}

function compare2(x, y) {
  switch (x) {
    case /* A */0 :
        if (y) {
          return false;
        } else {
          return true;
        }
    case /* B */1 :
        return y === 1;
    case /* C */2 :
        return y >= 2;
    
  }
}

function compare3(x, y) {
  return x === y;
}

exports.compare = compare;
exports.compare2 = compare2;
exports.compare3 = compare3;
/* No side effect */
