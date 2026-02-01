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

// 基础 MoonBit 核心测试用例 - 8个简单测试

test "basic arithmetic" {
  let x = 10
  let y = 5
  let sum = x + y
  let diff = x - y
  let prod = x * y
  let quot = x / y
  let rem = x % y
  @test.assert_eq(sum, 15)
  @test.assert_eq(diff, 5)
  @test.assert_eq(prod, 50)
  @test.assert_eq(quot, 2)
  @test.assert_eq(rem, 0)
}

test "string operations" {
  let s1 = "Hello"
  let s2 = "World"
  let combined = s1 + " " + s2
  let length = combined.length()
  @test.assert_eq(combined, "Hello World")
  @test.assert_eq(length, 11)
}

test "boolean logic" {
  let t = true
  let f = false
  let and_result = t && f
  let or_result = t || f
  let not_t = !t
  let not_f = !f
  @test.assert_eq(and_result, false)
  @test.assert_eq(or_result, true)
  @test.assert_eq(not_t, false)
  @test.assert_eq(not_f, true)
}

test "comparisons" {
  let a = 10
  let b = 20
  let c = 10
  let lt = a < b
  let gt = b > a
  let le = a <= c
  let ge = a >= c
  let eq = a == c
  let ne = a != b
  @test.assert_eq(lt, true)
  @test.assert_eq(gt, true)
  @test.assert_eq(le, true)
  @test.assert_eq(ge, true)
  @test.assert_eq(eq, true)
  @test.assert_eq(ne, true)
}

test "option type" {
  let some : Int? = Some(42)
  let none : Int? = None
  let some_unwrap = some.unwrap_or(0)
  let none_unwrap = none.unwrap_or(0)
  let some_map = some.map(x => x * 2)
  let none_map = none.map(x => x * 2)
  @test.assert_eq(some_unwrap, 42)
  @test.assert_eq(none_unwrap, 0)
  @test.assert_eq(some_map, Some(84))
  @test.assert_eq(none_map, None)
}

test "array operations" {
  let arr = [1, 2, 3, 4, 5]
  let len = arr.length()
  let first = arr[0]
  let last = arr[4]
  let doubled = arr.map(x => x * 2)
  @test.assert_eq(len, 5)
  @test.assert_eq(first, 1)
  @test.assert_eq(last, 5)
  @test.assert_eq(doubled, [2, 4, 6, 8, 10])
}

test "result type" {
  let ok : Result[Int, String] = Ok(42)
  let err : Result[Int, String] = Err("error")
  let ok_unwrap = ok.unwrap_or(0)
  let err_unwrap = err.unwrap_or(0)
  let ok_map = ok.map(x => x * 2)
  let err_map = err.map(x => x * 2)
  @test.assert_eq(ok_unwrap, 42)
  @test.assert_eq(err_unwrap, 0)
  @test.assert_eq(ok_map, Ok(84))
  @test.assert_eq(err_map, Err("error"))
}

test "tuple operations" {
  let pair = (10, "hello")
  let triple = (1, 2, 3)
  let (first, second) = pair
  let t0 = triple.0
  let t1 = triple.1
  let t2 = triple.2
  @test.assert_eq(first, 10)
  @test.assert_eq(second, "hello")
  @test.assert_eq(t0, 1)
  @test.assert_eq(t1, 2)
  @test.assert_eq(t2, 3)
}