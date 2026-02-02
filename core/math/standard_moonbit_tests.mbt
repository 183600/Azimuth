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
test "cube root calculations" {
  // 测试立方根计算
  inspect(@math.cbrtf(0), content="0")
  inspect(@math.cbrtf(1), content="1")
  inspect(@math.cbrtf(8), content="2")
  inspect(@math.cbrtf(27), content="3")
  inspect(@math.cbrtf(64), content="4")
  inspect(@math.cbrtf(125), content="5")
  
  // 测试负数的立方根
  inspect(@math.cbrtf(-1), content="-1")
  inspect(@math.cbrtf(-8), content="-2")
  inspect(@math.cbrtf(-27), content="-3")
  
  // 测试小数的立方根
  inspect(@math.cbrtf(0.125), content="0.5")
  inspect(@math.cbrtf(0.0625), content="0.39764277696609497")
}

///|
test "hypotenuse calculations" {
  // 测试直角三角形斜边计算
  inspect(@math.hypotf(0, 0), content="0")
  inspect(@math.hypotf(3, 4), content="5")
  inspect(@math.hypotf(5, 12), content="13")
  inspect(@math.hypotf(8, 15), content="17")
  inspect(@math.hypotf(7, 24), content="25")
  
  // 测试相同数值的斜边
  inspect(@math.hypotf(1, 1), content="1.4142135381698608")
  inspect(@math.hypotf(2, 2), content="2.8284270763397217")
  inspect(@math.hypotf(10, 10), content="14.142135620117188")
  
  // 测试一边为0的情况
  inspect(@math.hypotf(0, 5), content="5")
  inspect(@math.hypotf(7, 0), content="7")
}

///|
test "mathematical constants" {
  // 测试数学常数
  inspect(@float.pi, content="3.1415927410125732")
  inspect(@float.e, content="2.7182817459106445")
  
  // 测试无穷大
  inspect(@float.infinity, content="Infinity")
  inspect(@float.neg_infinity, content="-Infinity")
  
  // 测试非数字
  inspect(@float.not_a_number.is_nan(), content="true")
}

///|
test "float special operations" {
  // 测试浮点数特殊操作
  let normal_num = 3.14159
  let zero = 0.0
  let neg_num = -2.71828
  
  // 测试绝对值
  inspect(normal_num.abs(), content="3.1415927410125732")
  inspect(neg_num.abs(), content="2.7182817459106445")
  inspect(zero.abs(), content="0")
  
  // 测试平方根
  inspect(normal_num.sqrt(), content="1.7724538803100586")
  inspect(4.0.sqrt(), content="2")
  inspect(9.0.sqrt(), content="3")
  
  // 测试特殊值的平方根
  inspect(@float.infinity.sqrt(), content="Infinity")
  inspect(@float.not_a_number.sqrt().is_nan(), content="true")
}