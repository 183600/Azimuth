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
test "basic arithmetic operations" {
  let a = 10
  let b = 5
  let sum = a + b
  let product = a * b
  let difference = a - b
  
  inspect(sum, content="15")
  inspect(product, content="50")
  inspect(difference, content="5")
}

///|
test "boolean operations" {
  let true_val = true
  let false_val = false
  let and_result = true_val && false_val
  let or_result = true_val || false_val
  let not_result = !true_val
  
  inspect(and_result, content="false")
  inspect(or_result, content="true")
  inspect(not_result, content="false")
}

///|
test "string operations" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  let empty = ""
  
  inspect(combined, content="Hello World")
  inspect(empty, content="")
  inspect(hello.length(), content="5")
}

///|
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[4]
  let length = numbers.length()
  
  inspect(first, content="1")
  inspect(last, content="5")
  inspect(length, content="5")
}

///|
test "option type operations" {
  let some_value : Int? = Some(42)
  let no_value : Int? = None
  let extracted = match some_value {
    Some(x) => x
    None => 0
  }
  
  inspect(extracted, content="42")
  inspect(some_value is Some(_), content="true")
  inspect(no_value is None, content="true")
}

///|
test "result type operations" {
  let success : Result[Int, String] = Ok(42)
  let failure : Result[Int, String] = Err("error")
  let success_value = match success {
    Ok(x) => x
    Err(_) => 0
  }
  
  inspect(success_value, content="42")
  inspect(success is Ok(_), content="true")
  inspect(failure is Err(_), content="true")
}

///|
test "comparison operations" {
  let a = 10
  let b = 5
  let c = 10
  
  inspect(a > b, content="true")
  inspect(a < b, content="false")
  inspect(a == c, content="true")
  inspect(a != b, content="true")
  inspect(a >= c, content="true")
  inspect(b <= a, content="true")
}

///|
test "modulo operations" {
  let a = 10
  let b = 3
  let remainder = a % b
  
  inspect(remainder, content="1")
  inspect(0 % 5, content="0")
  inspect(5 % 5, content="0")
}

///|
test "negative number operations" {
  let neg = -10
  let pos = 10
  let sum = neg + pos
  let abs_neg = -neg
  
  inspect(sum, content="0")
  inspect(abs_neg, content="10")
  inspect(neg < 0, content="true")
}

///|
test "nested operations" {
  let a = 2
  let b = 3
  let c = 4
  
  let result1 = (a + b) * c
  let result2 = a * b + c
  let result3 = a * (b + c)
  
  inspect(result1, content="20")
  inspect(result2, content="10")
  inspect(result3, content="14")
}