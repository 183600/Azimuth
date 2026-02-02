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

test "int bitwise operations comprehensive" {
  // Test complex bitwise operations
  inspect((0xFF & 0x0F), content="15")
  inspect((0xF0 | 0x0F), content="255")
  inspect((0xFF ^ 0xAA), content="85")
  
  // Test bitwise NOT with specific patterns
  inspect(~0, content="-1")
  inspect(~(-1), content="0")
  inspect(~0xFF, content="-256")
  
  // Test shift operations with edge cases
  inspect((1 << 31), content="-2147483648")
  inspect((-2147483648 >> 31), content="-1")
}

///|
test "int bitwise operations comprehensive" {
  // Test complex bitwise operations
  assert_eq((0xFF & 0x0F), 0x0F)
  assert_eq((0xF0 | 0x0F), 0xFF)
  assert_eq((0xFF ^ 0xAA), 0x55)
  
  // Test bitwise NOT with specific patterns
  assert_eq(~0, -1)
  assert_eq(~(-1), 0)
  assert_eq(~0xFF, -256)
  
  // Test shift operations with edge cases
  assert_eq((1 << 31), @int.min_value)
  assert_eq((@int.min_value >> 31), -1)
}

///|
test "string operations basic" {
  // Test string concatenation
  let str1 = "Hello"
  let str2 = " World"
  let combined = str1 + str2
  inspect(combined, content="Hello World")
  
  // Test string length
  inspect(combined.length(), content="11")
  
  // Test string comparison
  inspect("abc" == "abc", content="true")
  inspect("abc" != "def", content="true")
  inspect("abc" < "abd", content="true")
}

///|
test "list basic operations" {
  // Test list creation and operations
  let empty_list = []
  let list1 = [1, 2, 3]
  
  // Test list length
  inspect(list1.length(), content="3")
  
  // Test list head and tail
  inspect(list1[0], content="1")
  inspect(list1.length(), content="3")
  
  // Test list append
  let list2 = list1.concat([4])
  inspect(list2.length(), content="4")
}

///|
test "result error handling" {
  // Test Ok result operations
  let ok_result = Ok(42)
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_err(), content="false")
  inspect(ok_result.unwrap(), content="42")
  inspect(ok_result.unwrap_or(0), content="42")
  
  // Test Err result operations
  let err_result = Err("error message")
  inspect(err_result.is_ok(), content="false")
  inspect(err_result.is_err(), content="true")
  inspect(err_result.unwrap_or(0), content="0")
}

///|
test "result map operations" {
  // Test map on Ok result
  let ok_result = Ok(42)
  let mapped_ok = ok_result.map(x => x * 2)
  inspect(mapped_ok, content="Ok(84)")
  
  // Test map on Err result
  let err_result = Err("error")
  let mapped_err = err_result.map(x => x * 2)
  inspect(mapped_err, content="Err(error)")
  
  // Test map_err on Err result
  let mapped_err_msg = err_result.map_err(s => s + " occurred")
  inspect(mapped_err_msg, content="Err(error occurred)")
}

///|
test "int type conversion edge cases" {
  // Test conversion to different types
  let positive = 123456
  let negative = -123456
  
  // Test to_string conversion
  inspect(positive.to_string(), content="123456")
  inspect(negative.to_string(), content="-123456")
  
  // Test to_double with precision
  let double_pos = positive.to_double()
  let double_neg = negative.to_double()
  inspect(double_pos > 123455.0, content="true")
  inspect(double_pos < 123457.0, content="true")
  inspect(double_neg > -123457.0, content="true")
  inspect(double_neg < -123455.0, content="true")
}

///|
test "list functional operations" {
  // Create test list
  let numbers = [1, 2, 3, 4, 5]
  
  // Test filter
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens.length(), content="2")
  inspect(evens[0], content="2")
  
  // Test map
  let doubled = numbers.map(x => x * 2)
  inspect(doubled[0], content="2")
  inspect(doubled.length(), content="5")
  
  // Test fold/reduce
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

///|
test "option type operations" {
  // Test Some operations
  let some_value = Some(42)
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(some_value.unwrap(), content="42")
  inspect(some_value.unwrap_or(0), content="42")
  
  // Test None operations
  let none_value = None
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  inspect(none_value.unwrap_or(0), content="0")
  
  // Test map on Option
  let mapped_some = some_value.map(x => x * 2)
  inspect(mapped_some, content="Some(84)")
  
  let mapped_none = none_value.map(x => x * 2)
  inspect(mapped_none, content="None")
}

///|
test "comprehensive error propagation" {
  // Test chained operations with Results
  let divide = (a : Int, b : Int) -> Result[Int, String] => {
    if b == 0 { Err("Division by zero") } else { Ok(a / b) }
  }
  
  let safe_divide_and_double = (a : Int, b : Int) -> Result[Int, String] => {
    divide(a, b).and_then(x => Ok(x * 2))
  }
  
  // Test successful case
  let result1 = safe_divide_and_double(10, 2)
  inspect(result1, content="Ok(10)")
  
  // Test error case
  let result2 = safe_divide_and_double(10, 0)
  inspect(result2, content="Err(Division by zero)")
}