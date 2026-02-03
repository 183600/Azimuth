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
test "string_operations" {
  let str1 = "Hello"
  let str2 = "MoonBit"
  let combined = str1 + " " + str2
  inspect(combined, content="Hello MoonBit")
  inspect(combined.length(), content="12")
  inspect(String::default(), content="")
  inspect("".length(), content="0")
}

///|
test "array_basic_operations" {
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  let empty = []
  inspect(empty.length(), content="0")
  
  let strings = ["a", "b", "c"]
  inspect(strings.length(), content="3")
  inspect(strings[1], content="b")
}

///|
test "boolean_logic" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

///|
test "option_type_handling" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  let doubled = some_value.map(x => x * 2)
  inspect(doubled, content="Some(84)")
  
  let unwrapped = some_value.unwrap_or(0)
  let default_val = none_value.unwrap_or(0)
  inspect(unwrapped, content="42")
  inspect(default_val, content="0")
}

///|
test "result_error_handling" {
  let success : Result[String, String] = Ok("success")
  let failure : Result[String, String] = Err("error")
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
  
  let mapped_success = success.map(s => s.uppercase())
  inspect(mapped_success, content="Ok(\"SUCCESS\")")
  
  let mapped_failure = failure.map(s => s.uppercase())
  inspect(mapped_failure, content="Err(\"error\")")
}

///|
test "int_arithmetic_and_boundary" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  inspect(10 + 5, content="15")
  inspect(10 - 3, content="7")
  inspect(6 * 7, content="42")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  
  inspect(Int::abs(42), content="42")
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
}

///|
test "int_bit_operations" {
  inspect(5 & 3, content="1")
  inspect(5 | 3, content="7")
  inspect(5 ^ 3, content="6")
  inspect(~5, content="-6")
  inspect(5 << 2, content="20")
  inspect(20 >> 2, content="5")
  
  inspect(1 << 30, content="1073741824")
  inspect(1 << 31, content="-2147483648")
}

///|
test "char_and_unicode" {
  let ascii_char = 'A'
  let unicode_char = '世'
  
  inspect(ascii_char.to_string(), content="A")
  inspect(unicode_char.to_string(), content="世")
  
  inspect('a' < 'b', content="true")
  inspect('A' < 'a', content="true")
  inspect('0' < '9', content="true")
  
  let unicode_str = "Hello 世界"
  inspect(unicode_str.length(), content="8")
}

///|
test "array_functional_operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="[2, 4]")
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  let product = numbers.fold(1, (acc, x) => acc * x)
  inspect(product, content="120")
}

///|
test "type_conversions" {
  let int_val = 42
  let uint_val = int_val.reinterpret_as_uint()
  
  inspect(uint_val.to_int(), content="42")
  inspect(int_val.to_string(), content="42")
  inspect("123".to_int(), content="Some(123)")
  inspect("abc".to_int(), content="None")
  
  inspect(int_val.to_int64(), content="42")
  inspect(int_val.to_double(), content="42.0")
}