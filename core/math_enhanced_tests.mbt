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
test "math basic arithmetic functions" {
  // Test basic square root
  inspect(@math.sqrtf(0), content="0")
  inspect(@math.sqrtf(1), content="1")
  inspect(@math.sqrtf(4), content="2")
  inspect(@math.sqrtf(9), content="3")
  inspect(@math.sqrtf(16), content="4")
  
  // Test absolute values
  inspect(@math.fabsf(5.0), content="5")
  inspect(@math.fabsf(-5.0), content="5")
  inspect(@math.fabsf(0.0), content="0")
  
  // Test cube root
  inspect(@math.cbrtf(1), content="1")
  inspect(@math.cbrtf(8), content="2")
  inspect(@math.cbrtf(27), content="3")
  inspect(@math.cbrtf(-1), content="-1")
  inspect(@math.cbrtf(-8), content="-2")
}

///|
test "math power functions" {
  // Test power function
  inspect(@math.powf(2.0, 0), content="1")
  inspect(@math.powf(2.0, 1), content="2")
  inspect(@math.powf(2.0, 2), content="4")
  inspect(@math.powf(2.0, 3), content="8")
  inspect(@math.powf(3.0, 2), content="9")
  inspect(@math.powf(5.0, 3), content="125")
  
  // Test exponential function
  inspect(@math.expf(0), content="1")
  inspect(@math.expf(1), content="2.7182817459106445")
  
  // Test exponential function with base 2
  inspect(@math.exp2f(0), content="1")
  inspect(@math.exp2f(1), content="2")
  inspect(@math.exp2f(2), content="4")
  inspect(@math.exp2f(3), content="8")
}

///|
test "math logarithmic functions" {
  // Test natural logarithm
  inspect(@math.logf(1), content="0")
  inspect(@math.logf(@math.expf(1)), content="1")
  
  // Test logarithm base 10
  inspect(@math.log10f(1), content="0")
  inspect(@math.log10f(10), content="1")
  inspect(@math.log10f(100), content="2")
  inspect(@math.log10f(1000), content="3")
  
  // Test logarithm base 2
  inspect(@math.log2f(1), content="0")
  inspect(@math.log2f(2), content="1")
  inspect(@math.log2f(4), content="2")
  inspect(@math.log2f(8), content="3")
}

///|
test "math trigonometric functions" {
  // Test sine function
  inspect(@math.sinf(0), content="0")
  inspect(@math.sinf(@math.pi / 2), content="1")
  
  // Test cosine function
  inspect(@math.cosf(0), content="1")
  inspect(@math.cosf(@math.pi), content="-1")
  
  // Test tangent function
  inspect(@math.tanf(0), content="0")
  
  // Test arcsine function
  inspect(@math.asinf(0), content="0")
  inspect(@math.asinf(1), content="1.5707963705062866")
  
  // Test arccosine function
  inspect(@math.acosf(1), content="0")
  
  // Test arctangent function
  inspect(@math.atanf(0), content="0")
  inspect(@math.atanf(1), content="0.7853981852531433")
}

///|
test "math hyperbolic functions" {
  // Test hyperbolic sine
  inspect(@math.sinhf(0), content="0")
  
  // Test hyperbolic cosine
  inspect(@math.coshf(0), content="1")
  
  // Test hyperbolic tangent
  inspect(@math.tanhf(0), content="0")
}

///|
test "math rounding functions" {
  // Test floor function
  inspect(@math.floorf(3.7), content="3")
  inspect(@math.floorf(-3.7), content="-4")
  inspect(@math.floorf(3.0), content="3")
  
  // Test ceiling function
  inspect(@math.ceilf(3.2), content="4")
  inspect(@math.ceilf(-3.2), content="-3")
  inspect(@math.ceilf(3.0), content="3")
  
  // Test round function
  inspect(@math.roundf(3.2), content="3")
  inspect(@math.roundf(3.7), content="4")
  inspect(@math.roundf(-3.2), content="-3")
  inspect(@math.roundf(-3.7), content="-4")
}

///|
test "math special functions" {
  // Test hypotenuse function
  inspect(@math.hypotf(3, 4), content="5")
  inspect(@math.hypotf(5, 12), content="13")
  inspect(@math.hypotf(8, 15), content="17")
  
  // Test minimum and maximum
  inspect(@math.fminf(3.5, 7.2), content="3.5")
  inspect(@math.fminf(7.2, 3.5), content="3.5")
  inspect(@math.fmaxf(3.5, 7.2), content="7.2")
  inspect(@math.fmaxf(7.2, 3.5), content="7.2")
  
  // Test positive difference
  inspect(@math.fdimf(7.2, 3.5), content="3.7")
  inspect(@math.fdimf(3.5, 7.2), content="0")
}

///|
test "math constants" {
  // Test mathematical constants
  inspect(@math.pi, content="3.1415927410125732")
  inspect(@math.e, content="2.7182817459106445")
  
  // Test that pi is approximately correct
  let pi_approx = @math.pi
  let pi_error = @math.fabsf(pi_approx - 3.141592653589793)
  inspect(pi_error < 0.000001, content="true")
}