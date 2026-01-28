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
test "maximum with integers" {
  inspect(maximum(5, 3), content="5")
  inspect(maximum(-2, -8), content="-2")
  inspect(maximum(0, 0), content="0")
}

///|
test "minimum with integers" {
  inspect(minimum(5, 3), content="3")
  inspect(minimum(-2, -8), content="-8")
  inspect(minimum(0, 0), content="0")
}

///|
test "minmax returns ordered tuple" {
  let (min_val, max_val) = minmax(8, 3)
  inspect(min_val, content="3")
  inspect(max_val, content="8")
  
  let (min_val2, max_val2) = minmax(-5, -2)
  inspect(min_val2, content="-5")
  inspect(max_val2, content="-2")
}

///|
test "Reverse wrapper reverses comparison" {
  let a = Reverse(1)
  let b = Reverse(2)
  inspect(a.compare(b), content="1")  // 1 > 2 in reversed order
  inspect(b.compare(a), content="-1") // 2 < 1 in reversed order
  inspect(a.compare(a), content="0")  // Equality works correctly
}

///|
test "Reverse with strings" {
  let a = Reverse("apple")
  let b = Reverse("banana")
  inspect(a.compare(b), content="1")  // "apple" > "banana" in reversed order
  inspect(b.compare(a), content="-1") // "banana" < "apple" in reversed order
}

///|
test "maximum with floating point numbers" {
  inspect(maximum(3.14, 2.71), content="3.14")
  inspect(maximum(-1.5, -2.5), content="-1.5")
  inspect(maximum(0.0, -0.0), content="-0.0")  // -0.0 is considered greater in IEEE 754
}

///|
test "maximum_by_key with custom function" {
  let abs_fn = (x : Int) => if x < 0 { -x } else { x }
  inspect(maximum_by_key(5, -8, abs_fn), content="-8")
  inspect(maximum_by_key(-3, 2, abs_fn), content="-3")
  inspect(maximum_by_key(0, -5, abs_fn), content="-5")
}

///|
test "minimum_by_key with custom function" {
  let abs_fn = (x : Int) => if x < 0 { -x } else { x }
  inspect(minimum_by_key(5, -8, abs_fn), content="5")
  inspect(minimum_by_key(-3, 2, abs_fn), content="2")
  inspect(minimum_by_key(0, -5, abs_fn), content="0")
}

///|
test "minmax_by_key with custom function" {
  let abs_fn = (x : Int) => if x < 0 { -x } else { x }
  let (min_val, max_val) = minmax_by_key(5, -8, abs_fn)
  inspect(min_val, content="5")
  inspect(max_val, content="-8")
  
  let (min_val2, max_val2) = minmax_by_key(-3, 2, abs_fn)
  inspect(min_val2, content="2")
  inspect(max_val2, content="-3")
}

///|
test "minmax_by_key with string lengths" {
  let length_fn = (s : String) => s.length()
  let (short, long) = minmax_by_key("short", "very_long", length_fn)
  inspect(short, content="short")
  inspect(long, content="very_long")
  
  let (same1, same2) = minmax_by_key("abc", "def", length_fn)
  inspect(same1, content="abc")
  inspect(same2, content="def")
}