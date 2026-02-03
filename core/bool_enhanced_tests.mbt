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
test "bool basic logical operations" {
  let t = true
  let f = false
  
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(t && t, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
}

///|
test "bool comparison operations" {
  let t = true
  let f = false
  
  inspect(t == t, content="true")
  inspect(t == f, content="false")
  inspect(t != f, content="true")
  inspect(f != f, content="false")
}

///|
test "bool short-circuit evaluation" {
  let mut counter = 0
  
  let func_true = () => {
    counter = counter + 1
    true
  }
  
  let func_false = () => {
    counter = counter + 1
    false
  }
  
  // Test && short-circuit
  counter = 0
  let result1 = false && func_true()
  inspect(result1, content="false")
  inspect(counter, content="0")
  
  counter = 0
  let result2 = true && func_false()
  inspect(result2, content="false")
  inspect(counter, content="1")
  
  // Test || short-circuit
  counter = 0
  let result3 = true || func_false()
  inspect(result3, content="true")
  inspect(counter, content="0")
  
  counter = 0
  let result4 = false || func_true()
  inspect(result4, content="true")
  inspect(counter, content="1")
}

///|
test "bool to string conversion" {
  let t = true
  let f = false
  
  inspect(t.to_string(), content="true")
  inspect(f.to_string(), content="false")
}

///|
test "bool conditional expressions" {
  let t = true
  let f = false
  
  let result1 = if t { "yes" } else { "no" }
  let result2 = if f { "yes" } else { "no" }
  
  inspect(result1, content="yes")
  inspect(result2, content="no")
  
  let value1 = if t { 42 } else { 0 }
  let value2 = if f { 42 } else { 0 }
  
  inspect(value1, content="42")
  inspect(value2, content="0")
}

///|
test "bool array operations" {
  let bools = [true, false, true, true, false]
  
  // Count true values
  let true_count = bools.fold(0, (acc, b) => if b { acc + 1 } else { acc })
  inspect(true_count, content="3")
  
  // Count false values
  let false_count = bools.fold(0, (acc, b) => if b { acc } else { acc + 1 })
  inspect(false_count, content="2")
  
  // All true?
  let all_true = bools.fold(true, (acc, b) => acc && b)
  inspect(all_true, content="false")
  
  // Any true?
  let any_true = bools.fold(false, (acc, b) => acc || b)
  inspect(any_true, content="true")
}

///|
test "bool complex logical expressions" {
  let a = true
  let b = false
  let c = true
  
  // Complex combinations
  inspect((a && b) || (c && a), content="true")
  inspect((a || b) && (c || b), content="true")
  inspect(!(a && b) && c, content="true")
  inspect(!a || !b || !c, content="true")
  
  // De Morgan's laws
  let expr1 = !(a && b)
  let expr2 = !a || !b
  inspect(expr1 == expr2, content="true")
  
  let expr3 = !(a || b)
  let expr4 = !a && !b
  inspect(expr3 == expr4, content="true")
}

///|
test "bool ternary-like patterns" {
  let t = true
  let f = false
  
  // Using match expressions
  let result1 = match t {
    true => "success"
    false => "failure"
  }
  inspect(result1, content="success")
  
  let result2 = match f {
    true => "success"
    false => "failure"
  }
  inspect(result2, content="failure")
  
  // Nested conditions
  let complex_result = if t {
    if f { "both" } else { "first only" }
  } else {
    if f { "second only" } else { "neither" }
  }
  inspect(complex_result, content="first only")
}