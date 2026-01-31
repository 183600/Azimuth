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

// 标准 MoonBit 核心功能测试用例

///|
test "array creation and access" {
  let empty_array : Array[Int] = []
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["hello", "world", "moonbit"]
  
  inspect(empty_array.length(), content="0")
  inspect(numbers.length(), content="5")
  inspect(strings.length(), content="3")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  inspect(strings[1], content="\"world\"")
}

///|
test "option type operations" {
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  let some_string : String? = Some("test")
  
  inspect(some_int.is_some(), content="true")
  inspect(some_int.is_none(), content="false")
  inspect(none_int.is_some(), content="false")
  inspect(none_int.is_none(), content="true")
  
  let doubled = some_int.map(x => x * 2)
  inspect(doubled, content="Some(84)")
  
  let default_value = none_int.unwrap_or(0)
  inspect(default_value, content="0")
  
  let string_length = some_string.map(s => s.length())
  inspect(string_length, content="Some(4)")
}

///|
test "result type error handling" {
  let success : Result[Int, String] = Ok(100)
  let failure : Result[Int, String] = Err("error occurred")
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
  
  let mapped_success = success.map(x => x / 2)
  inspect(mapped_success, content="Ok(50)")
  
  let mapped_failure = failure.map(x => x / 2)
  inspect(mapped_failure, content="Err(\"error occurred\")")
  
  let recovered = failure.unwrap_or(0)
  inspect(recovered, content="0")
}

///|
test "string manipulation functions" {
  let greeting = "Hello"
  let target = "World"
  let message = greeting + ", " + target + "!"
  
  inspect(message, content="\"Hello, World!\"")
  inspect(message.length(), content="13")
  
  let empty = ""
  inspect(empty.length(), content="0")
  
  let single_char = "A"
  inspect(single_char.length(), content="1")
  
  let unicode = "中文测试"
  inspect(unicode.length(), content="4")
}

///|
test "integer arithmetic operations" {
  let a = 15
  let b = 3
  
  inspect(a + b, content="18")
  inspect(a - b, content="12")
  inspect(a * b, content="45")
  inspect(a / b, content="5")
  inspect(a % b, content="0")
  
  let zero = 0
  inspect(a + zero, content="15")
  inspect(a * zero, content="0")
  
  let negative = -5
  inspect(a + negative, content="10")
}

///|
test "boolean logic operations" {
  let true_val = true
  let false_val = false
  
  inspect(true_val && true_val, content="true")
  inspect(true_val && false_val, content="false")
  inspect(false_val && true_val, content="false")
  inspect(false_val && false_val, content="false")
  
  inspect(true_val || true_val, content="true")
  inspect(true_val || false_val, content="true")
  inspect(false_val || true_val, content="true")
  inspect(false_val || false_val, content="false")
  
  inspect(!true_val, content="false")
  inspect(!false_val, content="true")
}

///|
test "character operations" {
  let uppercase = 'A'
  let lowercase = 'a'
  let digit = '5'
  let space = ' '
  
  inspect(uppercase.to_string(), content="\"A\"")
  inspect(lowercase.to_string(), content="\"a\"")
  inspect(digit.to_string(), content="\"5\"")
  inspect(space.to_string(), content="\" \"")
  
  inspect(uppercase != lowercase, content="true")
  inspect(uppercase == uppercase, content="true")
  inspect(digit != space, content="true")
}

///|
test "functional array operations" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let doubled = numbers.map(x => x * 2)
  inspect(doubled.length(), content="10")
  inspect(doubled[0], content="2")
  inspect(doubled[9], content="20")
  
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens.length(), content="5")
  inspect(evens[0], content="2")
  inspect(evens[4], content="10")
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="55")
  
  let product = numbers.fold(1, (acc, x) => acc * x)
  inspect(product, content="3628800")
}

///|
test "option and result chaining" {
  let user_age : Option[Int] = Some(25)
  let no_age : Option[Int] = None
  
  let adult_check = user_age
    .map(age => if age >= 18 { "adult" } else { "minor" })
    .unwrap_or("unknown")
  inspect(adult_check, content="\"adult\"")
  
  let unknown_check = no_age
    .map(age => if age >= 18 { "adult" } else { "minor" })
    .unwrap_or("unknown")
  inspect(unknown_check, content="\"unknown\"")
  
  let calculation : Result[Int, String] = Ok(100)
  let error_result : Result[Int, String] = Err("division error")
  
  let final_result = calculation
    .map(x => x / 4)
    .map(x => x + 10)
    .unwrap_or(0)
  inspect(final_result, content="35")
  
  let error_final = error_result
    .map(x => x / 4)
    .map(x => x + 10)
    .unwrap_or(0)
  inspect(error_final, content="0")
}

///|
test "comparison and equality operations" {
  let int_a = 42
  let int_b = 42
  let int_c = 24
  
  inspect(int_a == int_b, content="true")
  inspect(int_a == int_c, content="false")
  inspect(int_a != int_c, content="true")
  inspect(int_a > int_c, content="true")
  inspect(int_c < int_b, content="true")
  inspect(int_a >= int_b, content="true")
  inspect(int_a <= int_b, content="true")
  
  let str_a = "hello"
  let str_b = "hello"
  let str_c = "world"
  
  inspect(str_a == str_b, content="true")
  inspect(str_a == str_c, content="false")
  inspect(str_a != str_c, content="true")
}