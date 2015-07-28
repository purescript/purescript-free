// module Control.Monad.Free

function List() {
  this.size = 0;
  this.head = null;
  this.last = null;
}

function Item(value) {
  this.value = value;
  this.next = null;
}

function append(list1) {
  return function(list2){
    if (list1.size === 0) return list2;
    else if (list2.size === 0) return list1;
    else {
      list1.last.next = list2.head;
      list1.last = list2.last;
      list1.size = list1.size + list2.size;
      return list1;
    }
  };
}

function empty() {
  return new List();
}

function $$null(list) {
  return list.size === 0;
}

function snoc(list) {
  return function(value){
    if (list.size === 0) {
      list.head = new Item(value);
      list.last = list.head;
      list.size = 1;
    }
    else {
      var item = new Item(value);
      var last = list.last;
      last.next = item;
      list.last = item;
      list.size = list.size + 1;
    }
    return list;
  };
}

function uncons$prime(nothing) {
  return function(just){
    return function(tuple){
      return function(list){
        if (list.size === 0) return nothing;
        else {
          var head = list.head;
          list.head = head.next;
          list.size = list.size - 1;
          return just(tuple(head.value)(list));
        }
      };
    };
  };
}

exports.append = append;

exports.empty = empty;

exports["null"] = $$null;

exports.snoc = snoc;

exports["uncons'"] = uncons$prime;
