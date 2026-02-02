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
test "option transpose for nested options" {
  let some_some : Option[Option[Int]] = Some(Some(42))
  let some_none : Option[Option[Int]] = Some(None)
  let none_some : Option[Option[Int]] = None
  
  // Manual transpose implementation
  let transpose = fn(x : Option[Option[Int]]) -> Option[Int] {
    match x {
      None => None
      Some(inner) => inner
    }
  }
  
  inspect(transpose(some_some), content="Some(42)")
  inspect(transpose(some_none), content="None")
  inspect(transpose(none_some), content="None")
}

///|
test "option contains functionality" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  let contains = fn(x : Option[Int], value : Int) -> Bool {
    match x {
      None => false
      Some(v) => v == value
    }
  }
  
  inspect(contains(some_value, 42), content="true")
  inspect(contains(some_value, 100), content="false")
  inspect(contains(none_value, 42), content="false")
}

///|
test "option zip operation" {
  let some1 : Option[Int] = Some(42)
  let some2 : Option[String] = Some("hello")
  let none1 : Option[Int] = None
  let none2 : Option[String] = None
  
  let zip = fn(x : Option[Int], y : Option[String]) -> Option[(Int, String)] {
    match (x, y) {
      (Some(a), Some(b)) => Some((a, b))
      _ => None
    }
  }
  
  inspect(zip(some1, some2), content="Some((42, \"hello\"))")
  inspect(zip(some1, none2), content="None")
  inspect(zip(none1, some2), content="None")
  inspect(zip(none1, none2), content="None")
}