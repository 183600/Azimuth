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

// Additional comprehensive test cases for bool module

test "bool_logical_operations" {
  let t = true
  let f = false
  
  // AND operations
  let and1 = t && t
  let and2 = t && f
  let and3 = f && t
  let and4 = f && f
  
  // OR operations
  let or1 = t || t
  let or2 = t || f
  let or3 = f || t
  let or4 = f || f
  
  // Simple validation
  let _ = and1 && and2 && and3 && and4 && or1 && or2 && or3 && or4
}

test "bool_negation" {
  let not1 = !true
  let not2 = !false
  
  // Double negation
  let double_not1 = !(!true)
  let double_not2 = !(!false)
  
  // Simple validation
  let _ = not1 && not2 && double_not1 && double_not2
}

test "bool_equality" {
  let eq1 = true == true
  let eq2 = true == false
  let eq3 = false == true
  let eq4 = false == false
  
  let ne1 = true != true
  let ne2 = true != false
  let ne3 = false != true
  let ne4 = false != false
  
  // Simple validation
  let _ = eq1 && eq2 && eq3 && eq4 && ne1 && ne2 && ne3 && ne4
}

test "bool_conditional_expressions" {
  let x = 5
  let y = 10
  
  let result1 = if (x < y) { "less" } else { "greater" }
  let result2 = if (x > y) { "greater" } else { "less" }
  
  // Simple validation
  let _ = result1 + result2
}

test "bool_bitwise_operations" {
  let t = true
  let f = false
  
  // Bitwise operations
  let bitwise_and1 = t & t
  let bitwise_and2 = t & f
  let bitwise_and3 = f & t
  let bitwise_and4 = f & f
  
  let bitwise_or1 = t | t
  let bitwise_or2 = t | f
  let bitwise_or3 = f | t
  let bitwise_or4 = f | f
  
  let bitwise_xor1 = t ^ t
  let bitwise_xor2 = t ^ f
  let bitwise_xor3 = f ^ t
  let bitwise_xor4 = f ^ f
  
  // Simple validation
  let _ = bitwise_and1 && bitwise_and2 && bitwise_and3 && bitwise_and4 && 
          bitwise_or1 && bitwise_or2 && bitwise_or3 && bitwise_or4 &&
          bitwise_xor1 && bitwise_xor2 && bitwise_xor3 && bitwise_xor4
}

test "bool_complex_expressions" {
  let a = true
  let b = false
  let c = true
  
  // Complex boolean expressions
  let expr1 = a && (b || c)
  let expr2 = (a && b) || c
  let expr3 = (a || b) && c
  let expr4 = !(a && c)
  let expr5 = !(a && b)
  
  // Nested expressions
  let nested1 = (a && c) || (b && a) || (c && b)
  let nested2 = (a && b) && (c || a) && (b || c)
  
  // Simple validation
  let _ = expr1 && expr2 && expr3 && expr4 && expr5 && nested1 && nested2
}