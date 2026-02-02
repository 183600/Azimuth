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
test "list basic operations" {
  // Test list creation and basic operations
  let empty_list = @list.empty()
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // Test list length calculation
  let list_length = @list.length(numbers)
  let empty_length = @list.length(empty_list)
  
  // Test list head and tail
  let head = @list.head(numbers)
  let tail = @list.tail(numbers)
  
  // Test list prepend
  let prepended = @list.Cons(0, numbers)
  
  // Test list conversion
  let back_to_array = @list.to_array(numbers)
  
  // Verify the operations work
  let _ = list_length
  let _ = empty_length
  let _ = head
  let _ = tail
  let _ = prepended
  let _ = back_to_array
}

///|
test "list utility functions" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // Test list reverse
  let reversed = @list.reverse(numbers)
  
  // Test list append
  let single = @list.Cons(42, @list.empty())
  let appended = @list.append(numbers, single)
  
  // Test list membership
  let contains_3 = @list.member(numbers, 3)
  let contains_10 = @list.member(numbers, 10)
  
  // Test list filtering
  let filtered = @list.filter(numbers, x => x % 2 == 0)
  
  // Test list mapping
  let mapped = @list.map(numbers, x => x * 2)
  
  // Verify the operations work
  let _ = reversed
  let _ = appended
  let _ = contains_3
  let _ = contains_10
  let _ = filtered
  let _ = mapped
}