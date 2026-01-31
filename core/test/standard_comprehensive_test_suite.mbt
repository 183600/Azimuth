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

///|
test "basic arithmetic" {
  let a = 10
  let b = 3
  inspect(a + b, content="13")
  inspect(a - b, content="7")
  inspect(a * b, content="30")
  inspect(a / b, content="3")
  inspect(a % b, content="1")
}

///|
test "string operations" {
  let s = "Hello"
  inspect(s.length(), content="5")
  inspect(s[0], content="'H'")
  
  let s2 = " World"
  let combined = s + s2
  inspect(combined, content="\"Hello World\"")
}

///|
test "boolean logic" {
  let t = true
  let f = false
  inspect(t && t, content="true")
  inspect(t || f, content="true")
  inspect(!f, content="true")
  inspect(t != f, content="true")
  inspect(t == f, content="false")
}

///|
test "array operations" {
  let arr = [1, 2, 3]
  inspect(arr.length(), content="3")
  inspect(arr[0], content="1")
  inspect(arr[2], content="3")
  
  let doubled = arr.map(x => x * 2)
  inspect(doubled.length(), content="3")
  inspect(doubled[0], content="2")
}

///|
test "option type" {
  let some_val : Option[Int] = Some(42)
  let none_val : Option[Int] = None
  
  let is_some = some_val is Some(_)
  let is_none = none_val is None
  
  inspect(is_some, content="true")
  inspect(is_none, content="true")
  
  let unwrapped = match some_val {
    Some(v) => v
    None => 0
  }
  inspect(unwrapped, content="42")
}

///|
test "result type" {
  let ok_val : Result[Int, String] = Ok(200)
  let err_val : Result[Int, String] = Err("error")
  
  let is_ok = ok_val is Ok(_)
  let is_err = err_val is Err(_)
  
  inspect(is_ok, content="true")
  inspect(is_err, content="true")
  
  let unwrapped = match ok_val {
    Ok(v) => v
    Err(_) => 0
  }
  inspect(unwrapped, content="200")
}

///|
test "comparisons" {
  inspect(10 < 20, content="true")
  inspect(30 > 15, content="true")
  inspect(25 <= 25, content="true")
  inspect(40 >= 40, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 6, content="true")
  
  inspect("apple" < "banana", content="true")
  inspect("test" == "test", content="true")
  inspect("test" != "Test", content="true")
}

///|
test "character operations" {
  let char_code = 'A'.to_int()
  inspect(char_code, content="65")
}

///|
test "array filtering" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let evens = numbers.filter(x => x % 2 == 0)
  let odds = numbers.filter(x => x % 2 == 1)
  let greater_than_5 = numbers.filter(x => x > 5)
  
  inspect(evens.length(), content="5")
  inspect(odds.length(), content="5")
  inspect(greater_than_5.length(), content="5")
  
  let first_even = evens[0]
  inspect(first_even, content="2")
}

///|
test "error handling" {
  fn safe_divide(a : Int, b : Int) -> Result[Int, String] {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  let good = safe_divide(10, 2)
  let bad = safe_divide(10, 0)
  
  let good_result = match good {
    Ok(v) => v
    Err(_) => 0
  }
  let bad_is_err = bad is Err(_)
  
  inspect(good_result, content="5")
  inspect(bad_is_err, content="true")
}