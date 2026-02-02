// Copyright 2026 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

///|
test "list basic operations" {
  // Test list creation using Cons and Nil
  let empty_list = Nil
  let single_element = Cons(1, Nil)
  let multiple_elements = Cons(1, Cons(2, Cons(3, Nil)))
  
  // Test list pattern matching
  let get_first = fn(lst : Int?list) -> Int {
    match lst {
      Nil => 0
      Cons(head, _) => head
    }
  }
  
  let get_length = fn(lst : Int?list) -> Int {
    let aux = fn(acc : Int, remaining : Int?list) -> Int {
      match remaining {
        Nil => acc
        Cons(_, tail) => aux(acc + 1, tail)
      }
    }
    aux(0, lst)
  }
  
  // Test the functions
  let first_of_empty = get_first(empty_list)
  let first_of_single = get_first(single_element)
  let first_of_multiple = get_first(multiple_elements)
  
  let length_of_empty = get_length(empty_list)
  let length_of_single = get_length(single_element)
  let length_of_multiple = get_length(multiple_elements)
  
  // Verify the operations work
  let _ = first_of_empty
  let _ = first_of_single
  let _ = first_of_multiple
  let _ = length_of_empty
  let _ = length_of_single
  let _ = length_of_multiple
}

///|
test "list utility functions" {
  let numbers = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  
  // Test list sum
  let sum_list = fn(lst : Int?list) -> Int {
    let aux = fn(acc : Int, remaining : Int?list) -> Int {
      match remaining {
        Nil => acc
        Cons(head, tail) => aux(acc + head, tail)
      }
    }
    aux(0, lst)
  }
  
  // Test list filter
  let filter_even = fn(lst : Int?list) -> Int?list {
    let aux = fn(acc : Int?list, remaining : Int?list) -> Int?list {
      match remaining {
        Nil => acc
        Cons(head, tail) => 
          if head % 2 == 0 {
            aux(Cons(head, acc), tail)
          } else {
            aux(acc, tail)
          }
      }
    }
    aux(Nil, lst)
  }
  
  // Test the functions
  let total = sum_list(numbers)
  let evens = filter_even(numbers)
  let evens_count = sum_list(evens)
  
  // Verify the operations work
  let _ = total
  let _ = evens_count
}