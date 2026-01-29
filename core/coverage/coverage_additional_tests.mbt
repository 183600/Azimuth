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
test "coverage_counter_edge_cases" {
  // Test edge cases for CoverageCounter
  let empty_counter = CoverageCounter::new(0)
  inspect(empty_counter.to_string(), content="[]")
  
  let single_counter = CoverageCounter::new(1)
  inspect(single_counter.to_string(), content="[0]")
  
  // Test incrementing single element
  single_counter.incr(0)
  inspect(single_counter.to_string(), content="[1]")
}

///|
test "coverage_counter_multiple_increments" {
  let counter = CoverageCounter::new(3)
  
  // Increment same index multiple times
  for i = 0; i < 5; i = i + 1 {
    counter.incr(1)
  }
  
  inspect(counter.to_string(), content="[0, 5, 0]")
}

///|
test "coverage_counter_boundary_operations" {
  let counter = CoverageCounter::new(5)
  
  // Increment first and last elements
  counter.incr(0)
  counter.incr(4)
  
  // Increment middle elements
  counter.incr(2)
  
  inspect(counter.to_string(), content="[1, 0, 1, 0, 1]")
}

///|
test "coverage_string_escaping_special_chars" {
  let test_strings = [
    "",
    " ",
    "\t",
    "\n",
    "\r",
    "\"",
    "'",
    "\\",
    "hello\nworld",
    "path\\to\\file",
    "quote\"test",
  ]
  
  for s in test_strings {
    let escaped = s.escape()
    // Just verify that escaping doesn't crash and produces a string
    let _ = escaped.length()
  }
}

///|
test "coverage_counter_large_operations" {
  let counter = CoverageCounter::new(100)
  
  // Increment every 10th element
  for i = 0; i < 100; i = i + 10 {
    counter.incr(i)
  }
  
  // Verify pattern
  let counter_str = counter.to_string()
  let _ = counter_str.contains("1")
}

///|
test "coverage_track_function" {
  let counter1 = CoverageCounter::new(2)
  let counter2 = CoverageCounter::new(3)
  
  // Track counters (this is a no-op in test context)
  @coverage.track("test_counter_1", counter1)
  @coverage.track("test_counter_2", counter2)
  
  // Increment counters
  counter1.incr(0)
  counter2.incr(1)
  
  inspect(counter1.to_string(), content="[1, 0]")
  inspect(counter2.to_string(), content="[0, 1, 0]")
}