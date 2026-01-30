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

// 高质量 MoonBit 标准库测试用例

///|
test "array boundary operations" {
  let arr = [1, 2, 3]
  let length = arr.length()
  inspect(length, content="3")
  inspect(arr[0], content="1")
  inspect(arr[2], content="3")
}

///|
test "option basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  let mapped = some_value.map(x => x * 2)
  inspect(mapped, content="Some(84)")
  
  let mapped_none = none_value.map(x => x * 2)
  inspect(mapped_none, content="None")
}

///|
test "result error handling" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_err(), content="false")
  inspect(err_result.is_ok(), content="false")
  inspect(err_result.is_err(), content="true")
  
  let mapped_ok = ok_result.map(x => x * 2)
  inspect(mapped_ok, content="Ok(84)")
  
  let mapped_err = err_result.map(x => x * 2)
  inspect(mapped_err, content="Err(\"error\")")
}

///|
test "string operations" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="Hello World")
  
  let unicode = "Hello, 世界"
  inspect(unicode.length(), content="8")
  inspect(unicode.char_at(0), content="'H'")
  inspect(unicode.char_at(7), content="'世'")
}

///|
test "int arithmetic and comparisons" {
  let a = 10
  let b = 20
  
  inspect(a + b, content="30")
  inspect(b - a, content="10")
  inspect(a * b, content="200")
  inspect(b / a, content="2")
  inspect(b % a, content="0")
  
  inspect(a < b, content="true")
  inspect(a > b, content="false")
  inspect(a == a, content="true")
  inspect(a != b, content="true")
}

///|
test "boolean logical operations" {
  let t = true
  let f = false
  
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
}

///|
test "char operations" {
  let ch = 'A'
  inspect(ch.to_string(), content="A")
  
  let lower = 'a'
  let upper = 'Z'
  inspect(lower <= upper, content="true")
  inspect(lower >= upper, content="false")
}

///|
test "array functional operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="[2, 4]")
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

///|
test "option chaining operations" {
  let user_data : Option[Int] = Some(25)
  
  let result = user_data
    .map(age => if age >= 18 { "adult" } else { "minor" })
    .unwrap_or("unknown")
  
  inspect(result, content="adult")
  
  let no_data : Option[Int] = None
  let result2 = no_data
    .map(age => if age >= 18 { "adult" } else { "minor" })
    .unwrap_or("unknown")
  
  inspect(result2, content="unknown")
}