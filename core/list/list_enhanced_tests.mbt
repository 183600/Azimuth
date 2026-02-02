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
test "list take functionality" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  let take = fn(lst : @list.List[Int], n : Int) -> @list.List[Int] {
    if n <= 0 {
      @list.empty()
    } else {
      match lst {
        Nil => @list.empty()
        Cons(head, tail) => @list.Cons(head, take(tail, n - 1))
      }
    }
  }
  
  let take_0 = take(numbers, 0)
  let take_2 = take(numbers, 2)
  let take_5 = take(numbers, 5)
  let take_10 = take(numbers, 10)
  
  inspect(take_0, content="@list.from_array([])")
  inspect(take_2, content="@list.from_array([1, 2])")
  inspect(take_5, content="@list.from_array([1, 2, 3, 4, 5])")
  inspect(take_10, content="@list.from_array([1, 2, 3, 4, 5])")
}

///|
test "list drop functionality" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  let drop = fn(lst : @list.List[Int], n : Int) -> @list.List[Int] {
    if n <= 0 {
      lst
    } else {
      match lst {
        Nil => @list.empty()
        Cons(_, tail) => drop(tail, n - 1)
      }
    }
  }
  
  let drop_0 = drop(numbers, 0)
  let drop_2 = drop(numbers, 2)
  let drop_5 = drop(numbers, 5)
  let drop_10 = drop(numbers, 10)
  
  inspect(drop_0, content="@list.from_array([1, 2, 3, 4, 5])")
  inspect(drop_2, content="@list.from_array([3, 4, 5])")
  inspect(drop_5, content="@list.from_array([])")
  inspect(drop_10, content="@list.from_array([])")
}

///|
test "list reverse functionality" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  let reverse = fn(lst : @list.List[Int]) -> @list.List[Int] {
    let aux = fn(acc : @list.List[Int], remaining : @list.List[Int]) -> @list.List[Int] {
      match remaining {
        Nil => acc
        Cons(head, tail) => aux(@list.Cons(head, acc), tail)
      }
    }
    aux(@list.empty(), lst)
  }
  
  let reversed_numbers = reverse(numbers)
  let reversed_empty = reverse(empty)
  
  inspect(reversed_numbers, content="@list.from_array([5, 4, 3, 2, 1])")
  inspect(reversed_empty, content="@list.from_array([])")
}