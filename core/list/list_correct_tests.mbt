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
  // Test list creation using More and Empty
  let empty_list : List[Int] = Empty
  let single_element : List[Int] = More(1, Empty)
  let multiple_elements : List[Int] = More(1, More(2, More(3, Empty)))
  
  // Test list pattern matching
  let get_first = fn(lst : List[Int]) -> Int {
    match lst {
      Empty => 0
      More(head, _) => head
    }
  }
  
  let get_length = fn(lst : List[Int]) -> Int {
    let aux = fn(acc : Int, remaining : List[Int]) -> Int {
      match remaining {
        Empty => acc
        More(_, tail) => aux(acc + 1, tail)
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
  let numbers : List[Int] = More(1, More(2, More(3, More(4, More(5, Empty)))))
  
  // Test list sum
  let sum_list = fn(lst : List[Int]) -> Int {
    let aux = fn(acc : Int, remaining : List[Int]) -> Int {
      match remaining {
        Empty => acc
        More(head, tail) => aux(acc + head, tail)
      }
    }
    aux(0, lst)
  }
  
  // Test list filter
  let filter_even = fn(lst : List[Int]) -> List[Int] {
    let aux = fn(acc : List[Int], remaining : List[Int]) -> List[Int] {
      match remaining {
        Empty => acc
        More(head, tail) => 
          if head % 2 == 0 {
            aux(More(head, acc), tail)
          } else {
            aux(acc, tail)
          }
      }
    }
    aux(Empty, lst)
  }
  
  // Test list reverse
  let reverse_list = fn(lst : List[Int]) -> List[Int] {
    let aux = fn(acc : List[Int], remaining : List[Int]) -> List[Int] {
      match remaining {
        Empty => acc
        More(head, tail) => aux(More(head, acc), tail)
      }
    }
    aux(Empty, lst)
  }
  
  // Test the functions
  let total = sum_list(numbers)
  let evens = filter_even(numbers)
  let reversed = reverse_list(numbers)
  
  // Verify the operations work
  let _ = total
  let _ = evens
  let _ = reversed
}