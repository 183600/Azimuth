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
test "Bool hash function" {
  inspect(Hasher::new(seed=0)..combine(true).finalize(), content="-205818221")
  inspect(Hasher::new(seed=0)..combine(false).finalize(), content="148298089")
}

///|
test "Bool hash function with struct" {
  struct TestHash {
    flag : Bool
    x : Int
  } derive(Hash, Eq, Show)
  // type MyInt Int derive(Show) this does not work
  // let m : Map[TestHash, Char] = {}
  let m = {}
  m[{ flag: true, x: 1 }] = '3'
  m[{ flag: false, x: 1 }] = '3'
  inspect(m, content="{{flag: true, x: 1}: '3', {flag: false, x: 1}: '3'}")
}

///|
test "bool convert" {
  inspect(true.to_int(), content="1")
  inspect(false.to_int(), content="0")
  inspect(true.to_int64(), content="1")
  inspect(false.to_int64(), content="0")
  inspect(true.to_uint(), content="1")
  inspect(false.to_uint(), content="0")
  inspect(true.to_uint64(), content="1")
  inspect(false.to_uint64(), content="0")
  inspect(true.to_uint16(), content="1")
  inspect(false.to_uint16(), content="0")
  inspect(true.to_int16(), content="1")
  inspect(false.to_int16(), content="0")
}

///|
test "bool logical operations" {
  // AND operations
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && true, content="false")
  inspect(false && false, content="false")
  
  // OR operations
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  
  // NOT operations
  inspect(!true, content="false")
  inspect(!false, content="true")
}

///|
test "bool comparison operations" {
  // Equality
  assert_eq(true, true)
  assert_eq(false, false)
  assert_ne(true, false)
  
  // Comparison with boolean logic
  inspect(true == true, content="true")
  inspect(true == false, content="false")
  inspect(false == true, content="false")
  inspect(false == false, content="true")
  
  inspect(true != false, content="true")
  inspect(false != true, content="true")
  inspect(true != true, content="false")
  inspect(false != false, content="false")
}

///|
test "bool bitwise operations" {
  // Bitwise AND
  inspect(true & true, content="true")
  inspect(true & false, content="false")
  inspect(false & true, content="false")
  inspect(false & false, content="false")
  
  // Bitwise OR
  inspect(true | true, content="true")
  inspect(true | false, content="true")
  inspect(false | true, content="true")
  inspect(false | false, content="false")
  
  // Bitwise XOR
  inspect(true ^ true, content="false")
  inspect(true ^ false, content="true")
  inspect(false ^ true, content="true")
  inspect(false ^ false, content="false")
}

///|
test "bool string conversion" {
  inspect(true.to_string(), content="\"true\"")
  inspect(false.to_string(), content="\"false\"")
}

///|
test "bool conditional expressions" {
  let x = 10
  let y = 20
  
  let result = if x < y { true } else { false }
  inspect(result, content="true")
  
  let result2 = if x > y { true } else { false }
  inspect(result2, content="false")
}

///|
test "bool in collections" {
  let bool_array = [true, false, true, false, true]
  inspect(bool_array.length(), content="5")
  inspect(bool_array[0], content="true")
  inspect(bool_array[1], content="false")
  
  let bool_list = @list.from_array([true, false, true])
  inspect(bool_list.length(), content="3")
  inspect(bool_list.head(), content="Some(true)")
}

///|
test "bool complex logical expressions" {
  let a = true
  let b = false
  let c = true
  
  // Complex combinations
  inspect(a && b || c, content="true")
  inspect(a || b && c, content="true")
  inspect((a || b) && (c || b), content="true")
  inspect(a && b && c, content="false")
  inspect(a || b || c, content="true")
  
  // Negated complex expressions
  inspect(!(a && b), content="true")
  inspect(!(a || b), content="false")
}
