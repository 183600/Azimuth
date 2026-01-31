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
test "bool_basic_logic" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && true, content="false")
  inspect(false && false, content="false")
}

///|
test "bool_or_logic" {
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
}

///|
test "bool_not_operations" {
  inspect(!true, content="false")
  inspect(!false, content="true")
  inspect(!(!true), content="true")
  inspect(!(!false), content="false")
}

///|
test "bool_equality" {
  inspect(true == true, content="true")
  inspect(false == false, content="true")
  inspect(true == false, content="false")
  inspect(false == true, content="false")
}

///|
test "bool_inequality" {
  inspect(true != false, content="true")
  inspect(false != true, content="true")
  inspect(true != true, content="false")
  inspect(false != false, content="false")
}

///|
test "bool_to_string_conversion" {
  inspect(true.to_string(), content="true")
  inspect(false.to_string(), content="false")
}

///|
test "bool_conditional_expressions" {
  let x = if (true) { 1 } else { 0 }
  let y = if (false) { 1 } else { 0 }
  
  inspect(x, content="1")
  inspect(y, content="0")
}