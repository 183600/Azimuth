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
test "result type error mapping and recovery" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("network error")
  
  // Test map_err function
  let mapped_err = err_result.map_err(msg => "Error: " + msg)
  inspect(mapped_err, content="Err(\"Error: network error\")")
  
  // Test or_else function for error recovery
  let recovery = fn(msg : String) -> Result[Int, String] {
    if msg == "network error" { Ok(0) } else { Err(msg) }
  }
  
  let recovered = err_result.or_else(recovery)
  inspect(recovered, content="Ok(0)")
  
  let unchanged = ok_result.or_else(recovery)
  inspect(unchanged, content="Ok(42)")
}

///|
test "int type arithmetic operations with overflow" {
  let x : Int = 1000000
  let y : Int = 2000000
  
  let sum = x + y
  let product = x * y
  let difference = y - x
  
  inspect(sum, content="3000000")
  inspect(product, content="2000000000000")
  inspect(difference, content="1000000")
  
  // Test division and remainder
  let dividend = 17
  let divisor = 5
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  inspect(quotient, content="3")
  inspect(remainder, content="2")
}

///|
test "string manipulation and formatting" {
  let first_name = "John"
  let last_name = "Doe"
  let age = 30
  
  let full_name = first_name + " " + last_name
  inspect(full_name, content="\"John Doe\"")
  
  let formatted = "Name: \{first_name}, Age: \{age}"
  inspect(formatted, content="\"Name: John, Age: 30\"")
  
  // Test string length
  let short = "hi"
  let long = "this is a longer string"
  inspect(short.length(), content="2")
  inspect(long.length(), content="22")
}

///|
test "option type chaining operations" {
  let user_id : Int? = Some(123)
  let empty_id : Int? = None
  
  // Simulate database lookup
  let lookup_user = fn(id : Int) -> String? {
    if id == 123 { Some("Alice") } else { None }
  }
  
  let get_user_role = fn(name : String) -> String? {
    if name == "Alice" { Some("admin") } else { None }
  }
  
  let user_role = user_id.and_then(lookup_user).and_then(get_user_role)
  inspect(user_role, content="Some(\"admin\")")
  
  let empty_role = empty_id.and_then(lookup_user).and_then(get_user_role)
  inspect(empty_role, content="None")
}

///|
test "list operations and transformations" {
  let numbers = [1, 2, 3, 4, 5]
  let empty_list : List[Int] = []
  
  // Test list length
  inspect(numbers.length(), content="5")
  inspect(empty_list.length(), content="0")
  
  // Test list head and tail
  inspect(numbers.head(), content="Some(1)")
  inspect(empty_list.head(), content="None")
  
  // Test list append
  let extended = numbers.push(6)
  inspect(extended, content="[1, 2, 3, 4, 5, 6]")
  
  // Test list filter
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="[2, 4]")
}

///|
test "array operations and indexing" {
  let numbers = Array.make(5, 0)
  numbers[0] = 10
  numbers[1] = 20
  numbers[2] = 30
  numbers[3] = 40
  numbers[4] = 50
  
  // Test array indexing
  inspect(numbers[0], content="10")
  inspect(numbers[2], content="30")
  inspect(numbers[4], content="50")
  
  // Test array length
  inspect(numbers.length(), content="5")
  
  // Test array slice
  let slice = Array.slice(numbers, 1, 4)
  inspect(slice, content="[20, 30, 40]")
}

///|
test "boolean logic and conditional expressions" {
  let true_val = true
  let false_val = false
  
  // Test basic boolean operations
  inspect(true_val && false_val, content="false")
  inspect(true_val || false_val, content="true")
  inspect(!true_val, content="false")
  inspect(!false_val, content="true")
  
  // Test conditional expressions
  let x = 10
  let y = 20
  let max = if x > y { x } else { y }
  inspect(max, content="20")
  
  let min = if x < y { x } else { y }
  inspect(min, content="10")
}

///|
test "character operations and conversions" {
  let char_a = 'a'
  let char_z = 'z'
  let char_A = 'A'
  
  // Test character comparison
  inspect(char_a < char_z, content="true")
  inspect(char_A < char_a, content="true")
  
  // Test character to string conversion
  let str_from_char = char_a.to_string()
  inspect(str_from_char, content="\"a\"")
  
  // Test character equality
  inspect(char_a == 'a', content="true")
  inspect(char_a == 'A', content="false")
}

///|
test "tuple operations and destructuring" {
  let pair = (42, "answer")
  let triple = (1, 2, 3)
  
  // Test tuple access
  inspect(pair.0, content="42")
  inspect(pair.1, content="\"answer\"")
  
  // Test tuple destructuring
  let (first, second) = pair
  inspect(first, content="42")
  inspect(second, content="\"answer\"")
  
  let (x, y, z) = triple
  inspect(x + y + z, content="6")
}

///|
test "complex nested operations" {
  let data : List[Result[Int, String]] = [Ok(1), Err("error1"), Ok(2), Err("error2"), Ok(3)]
  
  // Filter only successful results
  let successful = data.filter(r => r.is_ok())
  inspect(successful.length(), content="3")
  
  // Extract values from successful results
  let values = successful.map(r => r.unwrap())
  inspect(values, content="[Ok(1), Ok(2), Ok(3)]")
  
  // Chain operations: filter, map, then sum
  let sum_of_values = data
    .filter(r => r.is_ok())
    .map(r => r.unwrap())
    .reduce(0, (acc, val) => acc + val)
  
  inspect(sum_of_values, content="6")
}