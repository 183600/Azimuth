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
test "int basic operations" {
  assert_eq(5 + 3, 8)
  assert_eq(10 - 4, 6)
  assert_eq(3 * 4, 12)
  assert_eq(15 / 3, 5)
}

///|
test "int comparison" {
  assert_true(5 > 3)
  assert_true(2 < 8)
  assert_true(4 == 4)
  assert_true(6 != 3)
}

///|
test "string basic operations" {
  let str1 = "Hello"
  let str2 = "World"
  let result = str1 + " " + str2
  assert_eq_string(result, "Hello World")
  assert_eq(result.length(), 11)
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
}

///|
test "array push and pop" {
  let mutable_array = [1, 2, 3]
  mutable_array.push(4)
  assert_eq(mutable_array.length(), 4)
  assert_eq(mutable_array[3], 4)
  
  let popped = mutable_array.pop()
  assert_eq(popped, Some(4))
  assert_eq(mutable_array.length(), 3)
}

///|
test "result basic operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  assert_true(ok_result is Ok(_))
  assert_true(err_result is Err(_))
  assert_eq(ok_result.unwrap_or(0), 42)
  assert_eq(err_result.unwrap_or(0), 0)
}

///|
test "result map operations" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("error")
  
  let mapped_ok = ok_result.map(x => x * 2)
  assert_eq(mapped_ok, Ok(10))
  
  let mapped_err = err_result.map(x => x * 2)
  assert_eq(mapped_err, Err("error"))
}

///|
test "string character access" {
  let test_string = "MoonBit"
  assert_eq(test_string[0], 'M')
  assert_eq(test_string[3], 'n')
  assert_eq(test_string[6], 't')
}

///|
test "array map operation" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled.length(), 5)
  assert_eq(doubled[0], 2)
  assert_eq(doubled[4], 10)
}

///|
test "result pattern matching" {
  let ok_result : Result[Int, String] = Ok(200)
  let err_result : Result[Int, String] = Err("not found")
  
  let ok_message = match ok_result {
    Ok(value) => "Success: " + value.to_string()
    Err(error) => "Error: " + error
  }
  assert_eq_string(ok_message, "Success: 200")
  
  let err_message = match err_result {
    Ok(value) => "Success: " + value.to_string()
    Err(error) => "Error: " + error
  }
  assert_eq_string(err_message, "Error: not found")
}