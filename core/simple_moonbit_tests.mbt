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
test "basic arithmetic" {
  inspect(1 + 2, content="3")
  inspect(5 - 3, content="2")
  inspect(3 * 4, content="12")
  inspect(8 / 2, content="4")
}

///|
test "comparisons" {
  inspect(5 > 3, content="true")
  inspect(3 >= 3, content="true")
  inspect(2 < 4, content="true")
  inspect(4 <= 4, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

///|
test "string operations" {
  let s1 = "Hello"
  let s2 = "World"
  inspect(s1 + " " + s2, content="Hello World")
  inspect("abc" == "abc", content="true")
  inspect("abc" != "def", content="true")
}

///|
test "boolean logic" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

///|
test "unit type" {
  let u : Unit = ()
  inspect(u, content="()")
}

///|
test "negative numbers" {
  inspect(-5, content="-5")
  inspect(-10 + 3, content="-7")
  inspect(-2 * 3, content="-6")
}

///|
test "zero operations" {
  inspect(0 + 5, content="5")
  inspect(5 - 0, content="5")
  inspect(0 * 7, content="0")
  inspect(0 / 1, content="0")
}

///|
test "option type" {
  let some : Int? = Some(42)
  let none : Int? = None
  inspect(some.is_some(), content="true")
  inspect(some.is_none(), content="false")
  inspect(none.is_some(), content="false")
  inspect(none.is_none(), content="true")
}

///|
test "result type" {
  let ok : Result[Int, String] = Ok(42)
  let err : Result[Int, String] = Err("error")
  inspect(ok.is_ok(), content="true")
  inspect(ok.is_err(), content="false")
  inspect(err.is_ok(), content="false")
  inspect(err.is_err(), content="true")
}

///|
test "char operations" {
  let c = 'A'
  inspect(c.to_string(), content="\"A\"")
}