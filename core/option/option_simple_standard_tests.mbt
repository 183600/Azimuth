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
test "option basic creation" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

///|
test "option unwrap operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.unwrap(), content="42")
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
  
  inspect(some_value.unwrap_or_else(() => 100), content="42")
  inspect(none_value.unwrap_or_else(() => 100), content="100")
}

///|
test "option map operation" {
  let some_value : Int? = Some(21)
  let none_value : Int? = None
  
  let doubled_some = some_value.map(x => x * 2)
  inspect(doubled_some, content="Some(42)")
  
  let doubled_none = none_value.map(x => x * 2)
  inspect(doubled_none, content="None")
}

///|
test "option filter operation" {
  let some_positive : Int? = Some(10)
  let some_negative : Int? = Some(-5)
  let none_value : Int? = None
  
  let filtered_positive = some_positive.filter(x => x > 0)
  inspect(filtered_positive, content="Some(10)")
  
  let filtered_negative = some_negative.filter(x => x > 0)
  inspect(filtered_negative, content="None")
  
  let filtered_none = none_value.filter(x => x > 0)
  inspect(filtered_none, content="None")
}

///|
test "option and_then operation" {
  let some_value : Int? = Some(10)
  let none_value : Int? = None
  
  let safe_divide = (x : Int) => if x == 0 { None } else { Some(100 / x) }
  
  let result_some = some_value.and_then(safe_divide)
  inspect(result_some, content="Some(10)")
  
  let result_none = none_value.and_then(safe_divide)
  inspect(result_none, content="None")
  
  let zero_value : Int? = Some(0)
  let result_zero = zero_value.and_then(safe_divide)
  inspect(result_zero, content="None")
}

///|
test "option equality" {
  let some1 : Int? = Some(5)
  let some2 : Int? = Some(5)
  let some3 : Int? = Some(10)
  let none1 : Int? = None
  let none2 : Int? = None
  
  inspect(some1, content="Some(5)")
  inspect(some2, content="Some(5)")
  inspect(some3, content="Some(10)")
  inspect(none1, content="None")
  inspect(none2, content="None")
}

///|
test "option to_array conversion" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let array_from_some = some_value.to_array()
  inspect(array_from_some, content="[42]")
  
  let array_from_none = none_value.to_array()
  inspect(array_from_none, content="[]")
}

///|
test "option default" {
  let default_int : Int? = Option::default()
  let default_string : String? = Option::default()
  
  inspect(default_int, content="None")
  inspect(default_string, content="None")
}