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
test "queue basic operations" {
  let empty = @queue.new[Int]()
  let queue = @queue.new[Int]()
  
  assert_eq(empty.length(), 0)
  assert_eq(empty.is_empty(), true)
  
  queue.push(1)
  queue.push(2)
  queue.push(3)
  
  assert_eq(queue.length(), 3)
  assert_eq(queue.is_empty(), false)
}

///|
test "queue push and pop operations" {
  let queue = @queue.new[Int]()
  
  queue.push(10)
  queue.push(20)
  queue.push(30)
  
  let first = queue.pop()
  let second = queue.pop()
  let third = queue.pop()
  let fourth = queue.pop()
  
  assert_eq(first, Some(10))
  assert_eq(second, Some(20))
  assert_eq(third, Some(30))
  assert_eq(fourth, None[Int])
  
  assert_eq(queue.length(), 0)
  assert_eq(queue.is_empty(), true)
}

///|
test "queue peek operations" {
  let queue = @queue.new[Int]()
  
  assert_eq(queue.peek(), None[Int])
  assert_eq(queue.peek_last(), None[Int])
  
  queue.push(1)
  assert_eq(queue.peek(), Some(1))
  assert_eq(queue.peek_last(), Some(1))
  
  queue.push(2)
  assert_eq(queue.peek(), Some(1))
  assert_eq(queue.peek_last(), Some(2))
  
  queue.push(3)
  assert_eq(queue.peek(), Some(1))
  assert_eq(queue.peek_last(), Some(3))
  
  // Pop one element and check again
  let popped = queue.pop()
  assert_eq(popped, Some(1))
  assert_eq(queue.peek(), Some(2))
  assert_eq(queue.peek_last(), Some(3))
}

///|
test "queue from array conversion" {
  let array = [1, 2, 3, 4, 5]
  let queue = @queue.from_array(array)
  
  assert_eq(queue.length(), 5)
  assert_eq(queue.peek(), Some(1))
  assert_eq(queue.peek_last(), Some(5))
  
  // Verify order is preserved
  let first = queue.pop()
  let second = queue.pop()
  let third = queue.pop()
  let fourth = queue.pop()
  let fifth = queue.pop()
  
  assert_eq(first, Some(1))
  assert_eq(second, Some(2))
  assert_eq(third, Some(3))
  assert_eq(fourth, Some(4))
  assert_eq(fifth, Some(5))
}

///|
test "queue to array conversion" {
  let queue = @queue.new[Int]()
  
  queue.push(1)
  queue.push(2)
  queue.push(3)
  queue.push(4)
  queue.push(5)
  
  let array = queue.to_array()
  assert_eq(array, [1, 2, 3, 4, 5])
  
  // Test with empty queue
  let empty_queue = @queue.new[Int]()
  let empty_array = empty_queue.to_array()
  assert_eq(empty_array, [])
}

///|
test "queue clear operation" {
  let queue = @queue.new[Int]()
  
  queue.push(1)
  queue.push(2)
  queue.push(3)
  
  assert_eq(queue.length(), 3)
  assert_eq(queue.is_empty(), false)
  
  queue.clear()
  
  assert_eq(queue.length(), 0)
  assert_eq(queue.is_empty(), true)
  assert_eq(queue.peek(), None[Int])
  assert_eq(queue.peek_last(), None[Int])
}

///|
test "queue iteration" {
  let queue = @queue.new[Int]()
  
  queue.push(1)
  queue.push(2)
  queue.push(3)
  queue.push(4)
  queue.push(5)
  
  let mut sum = 0
  let mut count = 0
  
  for value in queue {
    sum = sum + value
    count = count + 1
  }
  
  assert_eq(sum, 15)
  assert_eq(count, 5)
  
  // Verify queue is unchanged after iteration
  assert_eq(queue.length(), 5)
  assert_eq(queue.peek(), Some(1))
}

///|
test "queue contains operation" {
  let queue = @queue.new[Int]()
  
  queue.push(10)
  queue.push(20)
  queue.push(30)
  queue.push(40)
  queue.push(50)
  
  assert_eq(queue.contains(10), true)
  assert_eq(queue.contains(30), true)
  assert_eq(queue.contains(50), true)
  assert_eq(queue.contains(15), false)
  assert_eq(queue.contains(0), false)
  
  // Test with empty queue
  let empty_queue = @queue.new[Int]()
  assert_eq(empty_queue.contains(10), false)
}

///|
test "queue mixed operations" {
  let queue = @queue.new[Int]()
  
  // Push some elements
  queue.push(100)
  queue.push(200)
  queue.push(300)
  
  // Pop some elements
  let first = queue.pop()
  assert_eq(first, Some(200))
  
  // Push more elements
  queue.push(400)
  queue.push(500)
  
  // Pop remaining elements
  let second = queue.pop()
  let third = queue.pop()
  let fourth = queue.pop()
  let fifth = queue.pop()
  
  assert_eq(second, Some(300))
  assert_eq(third, Some(400))
  assert_eq(fourth, Some(500))
  assert_eq(fifth, None[Int])
}

///|
test "queue with different types" {
  // Test with strings
  let string_queue = @queue.new[String]()
  
  string_queue.push("hello")
  string_queue.push("world")
  string_queue.push("test")
  
  assert_eq(string_queue.pop(), Some("hello"))
  assert_eq(string_queue.pop(), Some("world"))
  assert_eq(string_queue.pop(), Some("test"))
  assert_eq(string_queue.pop(), None[String])
  
  // Test with tuples
  let tuple_queue = @queue.new[(Int, String)]()
  
  tuple_queue.push((1, "one"))
  tuple_queue.push((2, "two"))
  
  assert_eq(tuple_queue.pop(), Some((1, "one")))
  assert_eq(tuple_queue.pop(), Some((2, "two")))
  assert_eq(tuple_queue.pop(), None[(Int, String)])
}

///|
test "queue large operations" {
  let queue = @queue.new[Int]()
  
  // Push many elements
  for i = 0; i < 1000; i = i + 1 {
    queue.push(i)
  }
  
  assert_eq(queue.length(), 1000)
  assert_eq(queue.peek(), Some(0))
  
  // Pop all elements
  let mut sum = 0
  while let Some(value) = queue.pop() {
    sum = sum + value
  }
  
  assert_eq(sum, 499500)  // Sum of 0..999
  assert_eq(queue.length(), 0)
  assert_eq(queue.is_empty(), true)
}