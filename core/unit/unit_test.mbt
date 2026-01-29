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
test "unit_default" {
  let unit_value = @unit.default()
  // Unit type can be compared with itself
  assert_eq(unit_value, ())
}

///|
test "unit_type_operations" {
  let unit1 = ()
  let unit2 = ()
  
  // Unit values should be equal
  assert_eq(unit1, unit2)
  
  // Unit can be used in functions
  let fn_returning_unit = () -> Unit {
    ()
  }
  
  let result = fn_returning_unit()
  assert_eq(result, ())
}

///|
test "unit_in_pattern_matching" {
  let unit_value = ()
  
  match unit_value {
    () => assert_eq(true, true)  // This branch should be taken
  }
}

///|
test "unit_in_collections" {
  let unit_array = [(), (), ()]
  assert_eq(unit_array.length(), 3)
  
  let unit_list = @list.from_array([(), ()])
  assert_eq(unit_list.length(), 2)
}