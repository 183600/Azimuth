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

// Import necessary types and functions
using @builtin { trait Show, type StringBuilder, type SourceLoc, physical_equal }

///|
fn[T : Show] debug_string(t : T) -> String {
  let buf = StringBuilder::new(size_hint=50)
  t.output(buf)
  buf.to_string()
}

///|
/// Assert referential equality of two values.
///
/// Returns Ok if the two arguments are the same object by reference, using
/// `physical_equal`; raise an Error otherwise. Certain objects may be equal by
/// value, but they are different objects in the memory. This function checks
/// the latter.
///
/// # Examples
///
/// ```skip
/// let a = "4" + "2"
/// let b = "4" + "2"
/// @test.same_object(a, a)
/// @test.is_not(a, b)
/// ```
#callsite(autofill(loc))
pub fn[T : Show] same_object(a : T, b : T, loc~ : SourceLoc) -> Unit raise {
  if !physical_equal(a, b) {
    let a = debug_string(a)
    let b = debug_string(b)
    fail("`\{a} is \{b}`", loc~)
  }
}

///|
/// Assert referential inequality of two values.
///
/// Returns Ok if the two arguments are NOT the same object by reference, using
/// `physical_equal`; raise an Error otherwise. Certain objects may be equal
/// by value, but they are different objects in the memory. This function
/// checks the latter.
///
/// # Examples
///
/// ```skip
/// let a = "4" + "2"
/// let b = "4" + "2"
/// @test.is_not(a, b)
/// @test.same_object(a, a)
/// ```
#callsite(autofill(loc))
#alias(is_not, deprecated)
pub fn[T : Show] not_same_object(a : T, b : T, loc~ : SourceLoc) -> Unit raise {
  if physical_equal(a, b) {
    let a = debug_string(a)
    let b = debug_string(b)
    fail("`!(\{a} is \{b})`", loc~)
  }
}

///|
/// Expected to be used in test blocks, don't catch this error.
///  
/// Produces an error message similar to @builtin.fail.
#callsite(autofill(loc))
pub fn[T] fail(msg : String, loc~ : SourceLoc) -> T raise {
  @builtin.fail(msg, loc~)
}