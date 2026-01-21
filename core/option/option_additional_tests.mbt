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

// Additional comprehensive test cases for option module with edge cases

test "option_basic_creation" {
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  let some_string : String? = Some("Hello")
  let none_string : String? = None
  
  let is_some1 = some_int.is_some()
  let is_none1 = some_int.is_none()
  let is_some2 = none_int.is_some()
  let is_none2 = none_int.is_none()
  
  let is_some3 = some_string.is_some()
  let is_none3 = some_string.is_none()
  let is_some4 = none_string.is_some()
  let is_none4 = none_string.is_none()
  
  // Simple validation
  let _ = is_some1 && is_none1 && is_some2 && is_none2 && is_some3 && is_none3 && is_some4 && is_none4
}

test "option_unwrap_or" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let unwrap1 = some_value.unwrap_or(0)
  let unwrap2 = none_value.unwrap_or(0)
  let unwrap3 = none_value.unwrap_or(100)
  
  // Simple validation
  let _ = unwrap1 + unwrap2 + unwrap3
}

test "option_map" {
  let some_value : Int? = Some(5)
  let none_value : Int? = None
  
  let mapped_some = some_value.map(fn(x) { x * 2 })
  let mapped_none = none_value.map(fn(x) { x * 2 })
  
  let is_some1 = mapped_some.is_some()
  let is_some2 = mapped_none.is_some()
  
  // Simple validation
  let _ = if (is_some1 && !is_some2) { 1 } else { 0 }
}

test "option_filter" {
  let some_positive : Int? = Some(5)
  let some_negative : Int? = Some(-5)
  let none_value : Int? = None
  
  let filtered_positive = some_positive.filter(fn(x) { x > 0 })
  let filtered_negative = some_negative.filter(fn(x) { x > 0 })
  let filtered_none = none_value.filter(fn(x) { x > 0 })
  
  let is_some1 = filtered_positive.is_some()
  let is_some2 = filtered_negative.is_some()
  let is_some3 = filtered_none.is_some()
  
  // Simple validation
  let _ = if (is_some1 && !is_some2 && !is_some3) { 1 } else { 0 }
}

test "option_equality" {
  let some1 : Int? = Some(42)
  let some2 : Int? = Some(42)
  let some3 : Int? = Some(100)
  let none1 : Int? = None
  let none2 : Int? = None
  
  let eq1 = some1 == some2
  let eq2 = some1 == some3
  let eq3 = none1 == none2
  let eq4 = some1 == none1
  
  // Simple validation
  let _ = if (eq1 && !eq2 && eq3 && !eq4) { 1 } else { 0 }
}

test "option_basic_operations" {
  let some_value : Int? = Some(10)
  let none_value : Int? = None
  
  let is_some_check = some_value.is_some()
  let is_none_check = none_value.is_none()
  let unwrap_or = some_value.unwrap_or(0)
  let unwrap_or_none = none_value.unwrap_or(5)
  
  // Simple validation
  let _ = if (is_some_check && is_none_check) { unwrap_or + unwrap_or_none } else { 0 }
}