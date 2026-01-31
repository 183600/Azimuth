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
test "math basic constants" {
  inspect(@math.pi, content="3.141592653589793")
  inspect(@math.e, content="2.718281828459045")
  inspect(@math.sqrt2, content="1.4142135623730951")
  inspect(@math.log2e, content="1.4426950408889634")
  inspect(@math.log10e, content="0.4342944819032518")
  inspect(@math.ln2, content="0.6931471805599453")
  inspect(@math.ln10, content="2.302585092994046")
}

///|
test "math basic functions" {
  assert_eq(@math.abs(5.0), 5.0)
  assert_eq(@math.abs(-5.0), 5.0)
  assert_eq(@math.abs(0.0), 0.0)
  
  assert_eq(@math.ceil(3.2), 4.0)
  assert_eq(@math.ceil(3.8), 4.0)
  assert_eq(@math.ceil(-3.2), -3.0)
  assert_eq(@math.ceil(-3.8), -3.0)
  assert_eq(@math.ceil(4.0), 4.0)
  
  assert_eq(@math.floor(3.2), 3.0)
  assert_eq(@math.floor(3.8), 3.0)
  assert_eq(@math.floor(-3.2), -4.0)
  assert_eq(@math.floor(-3.8), -4.0)
  assert_eq(@math.floor(4.0), 4.0)
  
  assert_eq(@math.round(3.2), 3.0)
  assert_eq(@math.round(3.8), 4.0)
  assert_eq(@math.round(-3.2), -3.0)
  assert_eq(@math.round(-3.8), -4.0)
  assert_eq(@math.round(4.0), 4.0)
  
  assert_eq(@math.trunc(3.2), 3.0)
  assert_eq(@math.trunc(3.8), 3.0)
  assert_eq(@math.trunc(-3.2), -3.0)
  assert_eq(@math.trunc(-3.8), -3.0)
  assert_eq(@math.trunc(4.0), 4.0)
}

///|
test "math power and root functions" {
  assert_eq(@math.sqrt(4.0), 2.0)
  assert_eq(@math.sqrt(9.0), 3.0)
  assert_eq(@math.sqrt(0.0), 0.0)
  assert_eq(@math.sqrt(1.0), 1.0)
  
  assert_eq(@math.cbrt(8.0), 2.0)
  assert_eq(@math.cbrt(-8.0), -2.0)
  assert_eq(@math.cbrt(0.0), 0.0)
  assert_eq(@math.cbrt(1.0), 1.0)
  
  assert_eq(@math.pow(2.0, 3.0), 8.0)
  assert_eq(@math.pow(3.0, 2.0), 9.0)
  assert_eq(@math.pow(5.0, 0.0), 1.0)
  assert_eq(@math.pow(5.0, 1.0), 5.0)
  assert_eq(@math.pow(2.0, -1.0), 0.5)
}

///|
test "math exponential and logarithmic functions" {
  assert_eq(@math.exp(0.0), 1.0)
  assert_eq(@math.exp(1.0), @math.e)
  assert_eq(@math.exp(@math.ln(2.0)), 2.0)
  
  assert_eq(@math.exp2(0.0), 1.0)
  assert_eq(@math.exp2(1.0), 2.0)
  assert_eq(@math.exp2(3.0), 8.0)
  
  assert_eq(@math.log(@math.e), 1.0)
  assert_eq(@math.log(1.0), 0.0)
  assert_eq(@math.log(@math.exp(2.0)), 2.0)
  
  assert_eq(@math.log2(1.0), 0.0)
  assert_eq(@math.log2(2.0), 1.0)
  assert_eq(@math.log2(8.0), 3.0)
  
  assert_eq(@math.log10(1.0), 0.0)
  assert_eq(@math.log10(10.0), 1.0)
  assert_eq(@math.log10(100.0), 2.0)
}

///|
test "math trigonometric functions" {
  // Test special angles
  assert_eq(@math.sin(0.0), 0.0)
  assert_eq(@math.cos(0.0), 1.0)
  
  // Test π/2
  let pi_over_2 = @math.pi / 2.0
  inspect(@math.sin(pi_over_2), content="1.0")
  inspect(@math.cos(pi_over_2), content="0.0")
  
  // Test π
  inspect(@math.sin(@math.pi), content="0.0")
  inspect(@math.cos(@math.pi), content="-1.0")
  
  // Test tangent
  assert_eq(@math.tan(0.0), 0.0)
  inspect(@math.tan(pi_over_4), content="1.0") where pi_over_4 = @math.pi / 4.0
}

///|
test "math inverse trigonometric functions" {
  assert_eq(@math.asin(0.0), 0.0)
  assert_eq(@math.acos(1.0), 0.0)
  assert_eq(@math.atan(0.0), 0.0)
  
  // Test π/4 values
  let sqrt2_over_2 = @math.sqrt2 / 2.0
  inspect(@math.asin(sqrt2_over_2), content="0.7853981633974483")  // π/4
  inspect(@math.acos(sqrt2_over_2), content="0.7853981633974483")  // π/4
  inspect(@math.atan(1.0), content="0.7853981633974483")  // π/4
}

///|
test "math hyperbolic functions" {
  assert_eq(@math.sinh(0.0), 0.0)
  assert_eq(@math.cosh(0.0), 1.0)
  assert_eq(@math.tanh(0.0), 0.0)
  
  // Test some basic properties
  let x = 1.0
  let sinh_x = @math.sinh(x)
  let cosh_x = @math.cosh(x)
  inspect(@math.cosh(x) * @math.cosh(x) - @math.sinh(x) * @math.sinh(x), content="1.0")
}

///|
test "math min and max functions" {
  assert_eq(@math.min(1.0, 2.0), 1.0)
  assert_eq(@math.min(2.0, 1.0), 1.0)
  assert_eq(@math.min(1.0, 1.0), 1.0)
  assert_eq(@math.min(-1.0, -2.0), -2.0)
  
  assert_eq(@math.max(1.0, 2.0), 2.0)
  assert_eq(@math.max(2.0, 1.0), 2.0)
  assert_eq(@math.max(1.0, 1.0), 1.0)
  assert_eq(@math.max(-1.0, -2.0), -1.0)
}

///|
test "math remainder functions" {
  assert_eq(@math.fmod(7.0, 3.0), 1.0)
  assert_eq(@math.fmod(-7.0, 3.0), -1.0)
  assert_eq(@math.fmod(7.0, -3.0), 1.0)
  assert_eq(@math.fmod(-7.0, -3.0), -1.0)
  
  assert_eq(@math.remainder(7.0, 3.0), 1.0)
  assert_eq(@math.remainder(-7.0, 3.0), -1.0)
  assert_eq(@math.remainder(7.0, -3.0), 1.0)
  assert_eq(@math.remainder(-7.0, -3.0), -1.0)
}

///|
test "math special functions" {
  assert_eq(@math.hypot(3.0, 4.0), 5.0)
  assert_eq(@math.hypot(0.0, 0.0), 0.0)
  assert_eq(@math.hypot(5.0, 0.0), 5.0)
  assert_eq(@math.hypot(0.0, 5.0), 5.0)
  
  // Test copysign
  assert_eq(@math.copysign(3.5, 2.0), 3.5)
  assert_eq(@math.copysign(3.5, -2.0), -3.5)
  assert_eq(@math.copysign(-3.5, 2.0), 3.5)
  assert_eq(@math.copysign(-3.5, -2.0), -3.5)
  
  // Test fdim
  assert_eq(@math.fdim(5.0, 3.0), 2.0)
  assert_eq(@math.fdim(3.0, 5.0), 0.0)
  assert_eq(@math.fdim(5.0, 5.0), 0.0)
}