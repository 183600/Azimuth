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

// Import necessary functions
using @builtin { inspect }

// 基本功能测试用例

///|
test "string_basic_operations" {
  let s = "Hello"
  inspect(s.length(), content="5")
  inspect(s[0], content="'H'")
}

///|
test "int_arithmetic_operations" {
  let a = 10
  let b = 3
  inspect(a + b, content="13")
  inspect(a - b, content="7")
  inspect(a * b, content="30")
  inspect(a / b, content="3")
  inspect(a % b, content="1")
}

///|
test "boolean_logic_operations" {
  let t = true
  let f = false
  inspect(t && t, content="true")
  inspect(t || f, content="true")
  inspect(!f, content="true")
  inspect(t != f, content="true")
}

///|
test "array_basic_operations" {
  let arr = [1, 2, 3]
  inspect(arr.length(), content="3")
  inspect(arr[0], content="1")
  inspect(arr[2], content="3")
}

///|
test "option_type_pattern_matching" {
  let some_val : Option[Int] = Some(42)
  let none_val : Option[Int] = None
  let result1 = match some_val {
    Some(x) => x
    None => 0
  }
  let result2 = match none_val {
    Some(x) => x
    None => 0
  }
  inspect(result1, content="42")
  inspect(result2, content="0")
}

///|
test "result_type_pattern_matching" {
  let ok_val : Result[Int, String] = Ok(42)
  let err_val : Result[Int, String] = Err("error")
  let result1 = match ok_val {
    Ok(x) => x
    Err(_) => 0
  }
  let result2 = match err_val {
    Ok(x) => x
    Err(_) => -1
  }
  inspect(result1, content="42")
  inspect(result2, content="-1")
}

///|
test "char_basic_operations" {
  let c = 'A'
  inspect(c.to_string(), content="\"A\"")
  inspect(c.to_int(), content="65")
}

///|
test "simple_pattern_matching" {
  let x = 5
  let result = match x {
    1 => "one"
    5 => "five"
    _ => "other"
  }
  inspect(result, content="\"five\"")
}

///|
test "tuple_operations" {
  let t = (42, "hello")
  inspect(t.0, content="42")
  inspect(t.1, content="\"hello\"")
}