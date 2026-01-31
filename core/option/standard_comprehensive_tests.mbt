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

using @builtin { inspect }

///|
test "option_basic_construction" {
  let some_value = @option.Some(42)
  let none_value = @option.None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

///|
test "option_value_extraction" {
  let some_value = @option.Some(42)
  let none_value : @option.Option[Int] = @option.None
  
  inspect(some_value.unwrap(), content="42")
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
}

///|
test "option_map_operations" {
  let some_value = @option.Some(5)
  let none_value : @option.Option[Int] = @option.None
  
  let mapped_some = some_value.map(fn(x) { x * 2 })
  let mapped_none = none_value.map(fn(x) { x * 2 })
  
  inspect(mapped_some.unwrap(), content="10")
  inspect(mapped_none.is_none(), content="true")
}

///|
test "option_filter_operations" {
  let some_value = @option.Some(5)
  let some_even = @option.Some(4)
  let none_value : @option.Option[Int] = @option.None
  
  let filtered_some = some_value.filter(fn(x) { x % 2 == 0 })
  let filtered_even = some_even.filter(fn(x) { x % 2 == 0 })
  let filtered_none = none_value.filter(fn(x) { x % 2 == 0 })
  
  inspect(filtered_some.is_none(), content="true")
  inspect(filtered_even.unwrap(), content="4")
  inspect(filtered_none.is_none(), content="true")
}

///|
test "option_and_then_operations" {
  let some_value = @option.Some(5)
  let none_value : @option.Option[Int] = @option.None
  
  let and_then_some = some_value.and_then(fn(x) { @option.Some(x * 2) })
  let and_then_none = none_value.and_then(fn(x) { @option.Some(x * 2) })
  
  inspect(and_then_some.unwrap(), content="10")
  inspect(and_then_none.is_none(), content="true")
}

///|
test "option_equality" {
  let some1 = @option.Some(42)
  let some2 = @option.Some(42)
  let some3 = @option.Some(24)
  let none1 : @option.Option[Int] = @option.None
  let none2 : @option.Option[Int] = @option.None
  
  inspect(some1 == some2, content="true")
  inspect(some1 == some3, content="false")
  inspect(none1 == none2, content="true")
  inspect(some1 == none1, content="false")
}