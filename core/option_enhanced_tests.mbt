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
test "option basic creation and pattern matching" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value, content="Some(42)")
  inspect(none_value, content="None")
  
  let result = match some_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(result, content="84")
  
  let none_result = match none_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(none_result, content="0")
}

///|
test "option unwrap operations" {
  let some_value : Int? = Some(123)
  let none_value : Int? = None
  
  inspect(some_value.unwrap(), content="123")
  inspect(some_value.unwrap_or(0), content="123")
  inspect(none_value.unwrap_or(0), content="0")
  inspect(some_value.unwrap_or(-1), content="123")
  inspect(none_value.unwrap_or(-1), content="-1")
}

///|
test "option is_some and is_none" {
  let some_value : String? = Some("hello")
  let none_value : String? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

///|
test "option map operation" {
  let some_value : Int? = Some(5)
  let none_value : Int? = None
  
  let mapped_some = some_value.map(x => x * 2)
  let mapped_none = none_value.map(x => x * 2)
  
  inspect(mapped_some, content="Some(10)")
  inspect(mapped_none, content="None")
  
  let string_some = some_value.map(x => x.to_string())
  inspect(string_some, content="Some(\"5\")")
}

///|
test "option filter operation" {
  let some_value : Int? = Some(10)
  let some_small : Int? = Some(3)
  let none_value : Int? = None
  
  let filtered_some = some_value.filter(x => x > 5)
  let filtered_small = some_small.filter(x => x > 5)
  let filtered_none = none_value.filter(x => x > 5)
  
  inspect(filtered_some, content="Some(10)")
  inspect(filtered_small, content="None")
  inspect(filtered_none, content="None")
}

///|
test "option and_then operation" {
  let some_value : Int? = Some(5)
  let none_value : Int? = None
  
  let and_then_some = some_value.and_then(x => if x > 3 { Some(x * 2) } else { None })
  let and_then_small = some_value.and_then(x => if x > 10 { Some(x * 2) } else { None })
  let and_then_none = none_value.and_then(x => Some(x * 2))
  
  inspect(and_then_some, content="Some(10)")
  inspect(and_then_small, content="None")
  inspect(and_then_none, content="None")
}

///|
test "option or operation" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  let alternative : Int? = Some(99)
  let none_alternative : Int? = None
  
  inspect(some_value.or(alternative), content="Some(42)")
  inspect(none_value.or(alternative), content="Some(99)")
  inspect(none_value.or(none_alternative), content="None")
}

///|
test "option default values" {
  let int_default : Int? = Option::default()
  let string_default : String? = Option::default()
  let bool_default : Bool? = Option::default()
  
  inspect(int_default, content="None")
  inspect(string_default, content="None")
  inspect(bool_default, content="None")
}