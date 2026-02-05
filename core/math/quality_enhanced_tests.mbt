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

// 高质量 MoonBit 测试用例 - Math 模块
// 包含10个全面的测试用例，覆盖核心数学功能和边界情况

///|
test "math_algebraic_functions_precision" {
  // 测试代数函数的精度
  // 立方根函数测试
  inspect(@math.cbrtf(0.0), content="0")
  inspect(@math.cbrtf(1.0), content="1")
  inspect(@math.cbrtf(8.0), content="2")
  inspect(@math.cbrtf(27.0), content="3")
  inspect(@math.cbrtf(64.0), content="4")
  inspect(@math.cbrtf(-1.0), content="-1")
  inspect(@math.cbrtf(-8.0), content="-2")
  inspect(@math.cbrtf(-27.0), content="-3")
  
  // 斜边函数测试
  inspect(@math.hypotf(3.0, 4.0), content="5")
  inspect(@math.hypotf(5.0, 12.0), content="13")
  inspect(@math.hypotf(8.0, 15.0), content="17")
  inspect(@math.hypotf(7.0, 24.0), content="25")
  inspect(@math.hypotf(0.0, 0.0), content="0")
}

///|
test "math_basic_arithmetic_comprehensive" {
  // 测试基本算术运算
  // 绝对值测试
  inspect(@math.abs(5.0), content="5")
  inspect(@math.abs(-5.0), content="5")
  inspect(@math.abs(0.0), content="0")
  inspect(@math.abs(-3.14159), content="3.14159")
  
  // 最小值和最大值测试
  inspect(@math.min(3.0, 7.0), content="3")
  inspect(@math.max(3.0, 7.0), content="7")
  inspect(@math.min(-2.0, -8.0), content="-8")
  inspect(@math.max(-2.0, -8.0), content="-2")
  inspect(@math.min(0.0, -1.0), content="-1")
  inspect(@math.max(0.0, -1.0), content="0")
  
  // 特殊值测试
  inspect(@math.min(@float.infinity, 100.0), content="100")
  inspect(@math.max(@float.neg_infinity, 100.0), content="100")
}

///|
test "math_rounding_functions_edge_cases" {
  // 测试舍入函数的边界情况
  // 向下取整测试
  inspect(@math.floor(3.7), content="3")
  inspect(@math.floor(-3.7), content="-4")
  inspect(@math.floor(3.0), content="3")
  inspect(@math.floor(-3.0), content="-3")
  inspect(@math.floor(0.0), content="0")
  inspect(@math.floor(-0.0), content="-0")
  
  // 向上取整测试
  inspect(@math.ceil(3.2), content="4")
  inspect(@math.ceil(-3.2), content="-3")
  inspect(@math.ceil(3.0), content="3")
  inspect(@math.ceil(-3.0), content="-3")
  inspect(@math.ceil(0.0), content="0")
  
  // 四舍五入测试
  inspect(@math.round(3.7), content="4")
  inspect(@math.round(3.2), content="3")
  inspect(@math.round(-3.7), content="-4")
  inspect(@math.round(-3.2), content="-3")
  inspect(@math.round(0.5), content="1")
  inspect(@math.round(-0.5), content="-1")
  
  // 截断测试
  inspect(@math.trunc(3.7), content="3")
  inspect(@math.trunc(-3.7), content="-3")
  inspect(@math.trunc(3.0), content="3")
  inspect(@math.trunc(-3.0), content="-3")
}

///|
test "math_power_root_functions_precision" {
  // 测试幂函数和根函数的精度
  // 平方根测试
  inspect(@math.sqrt(0.0), content="0")
  inspect(@math.sqrt(1.0), content="1")
  inspect(@math.sqrt(4.0), content="2")
  inspect(@math.sqrt(9.0), content="3")
  inspect(@math.sqrt(16.0), content="4")
  inspect(@math.sqrt(25.0), content="5")
  inspect(@math.sqrt(2.0), content="1.4142135623730951")
  
  // 幂函数测试
  inspect(@math.pow(2.0, 0.0), content="1")
  inspect(@math.pow(2.0, 1.0), content="2")
  inspect(@math.pow(2.0, 2.0), content="4")
  inspect(@math.pow(2.0, 3.0), content="8")
  inspect(@math.pow(3.0, 2.0), content="9")
  inspect(@math.pow(5.0, 3.0), content="125")
  inspect(@math.pow(10.0, 2.0), content="100")
  
  // 特殊情况测试
  inspect(@math.pow(0.0, 5.0), content="0")
  inspect(@math.pow(5.0, 0.0), content="1")
  inspect(@math.pow(-2.0, 3.0), content="-8")
  inspect(@math.pow(-2.0, 2.0), content="4")
}

///|
test "math_trigonometric_functions_accuracy" {
  // 测试三角函数的准确性
  // 正弦函数测试
  inspect(@math.sin(0.0), content="0")
  inspect(@math.sin(@math.pi / 2.0), content="1")
  inspect(@math.sin(@math.pi), content="0")
  inspect(@math.sin(3.0 * @math.pi / 2.0), content="-1")
  inspect(@math.sin(2.0 * @math.pi), content="0")
  
  // 余弦函数测试
  inspect(@math.cos(0.0), content="1")
  inspect(@math.cos(@math.pi / 2.0), content="0")
  inspect(@math.cos(@math.pi), content="-1")
  inspect(@math.cos(3.0 * @math.pi / 2.0), content="0")
  inspect(@math.cos(2.0 * @math.pi), content="1")
  
  // 正切函数测试
  inspect(@math.tan(0.0), content="0")
  inspect(@math.tan(@math.pi / 4.0), content="1")
  inspect(@math.tan(-@math.pi / 4.0), content="-1")
  
  // 角度转换测试
  inspect(@math.degrees(0.0), content="0")
  inspect(@math.degrees(@math.pi), content="180")
  inspect(@math.degrees(2.0 * @math.pi), content="360")
  inspect(@math.radians(0.0), content="0")
  inspect(@math.radians(180.0), content="3.141592653589793")
}

///|
test "math_logarithmic_exponential_functions" {
  // 测试对数和指数函数
  // 自然对数测试
  inspect(@math.log(1.0), content="0")
  inspect(@math.log(@math.e), content="1")
  inspect(@math.log(@math.e * @math.e), content="2")
  inspect(@math.log(10.0), content="2.302585092994046")
  
  // 以10为底的对数测试
  inspect(@math.log10(1.0), content="0")
  inspect(@math.log10(10.0), content="1")
  inspect(@math.log10(100.0), content="2")
  inspect(@math.log10(1000.0), content="3")
  
  // 以2为底的对数测试
  inspect(@math.log2(1.0), content="0")
  inspect(@math.log2(2.0), content="1")
  inspect(@math.log2(4.0), content="2")
  inspect(@math.log2(8.0), content="3")
  
  // 指数函数测试
  inspect(@math.exp(0.0), content="1")
  inspect(@math.exp(1.0), content="2.718281828459045")
  inspect(@math.exp(2.0), content="7.38905609893065")
}

///|
test "math_hyperbolic_functions_precision" {
  // 测试双曲函数的精度
  // 双曲正弦测试
  inspect(@math.sinh(0.0), content="0")
  inspect(@math.sinh(1.0), content="1.1752011936438014")
  inspect(@math.sinh(-1.0), content="-1.1752011936438014")
  
  // 双曲余弦测试
  inspect(@math.cosh(0.0), content="1")
  inspect(@math.cosh(1.0), content="1.543080634815244")
  inspect(@math.cosh(-1.0), content="1.543080634815244")
  
  // 双曲正切测试
  inspect(@math.tanh(0.0), content="0")
  inspect(@math.tanh(1.0), content="0.7615941559557649")
  inspect(@math.tanh(-1.0), content="-0.7615941559557649")
  
  // 双曲函数恒等式验证
  let x = 1.0
  let sinh_val = @math.sinh(x)
  let cosh_val = @math.cosh(x)
  let tanh_val = @math.tanh(x)
  assert_true((sinh_val / cosh_val).is_close(tanh_val))
}

///|
test "math_special_values_handling" {
  // 测试特殊值的处理
  let nan = @float.not_a_number
  let inf = @float.infinity
  let neg_inf = @float.neg_infinity
  
  // NaN操作测试
  assert_true(@math.sqrt(-1.0).is_nan())
  assert_true((nan + 1.0).is_nan())
  assert_true((nan * 5.0).is_nan())
  assert_true(@math.log(nan).is_nan())
  assert_true(@math.exp(nan).is_nan())
  
  // 无穷大操作测试
  assert_true(@math.log(0.0) == neg_inf)
  assert_true(@math.abs(neg_inf) == inf)
  assert_true(@math.exp(inf) == inf)
  assert_true(@math.exp(neg_inf) == 0.0)
  assert_true(@math.sqrt(inf) == inf)
  
  // 特殊值的算术运算
  assert_true((inf + 100.0) == inf)
  assert_true((inf - inf).is_nan())
  assert_true((inf * 0.0).is_nan())
  assert_true((inf / inf).is_nan())
}

///|
test "math_floating_point_precision" {
  // 测试浮点精度处理
  // 极小值测试
  let tiny = 1.0e-10
  inspect(@math.sqrt(tiny), content="1.0e-5")
  inspect(@math.log(tiny), content="-23.02585092994046")
  inspect(@math.exp(tiny), content="1.0000000001000001")
  
  // 极大值测试
  let large = 1.0e10
  inspect(@math.sqrt(large), content="100000")
  inspect(@math.log(large), content="23.02585092994046")
  
  // 精度边界测试
  inspect(@math.sin(1.0e16), content="-0.2879033166650653")
  inspect(@math.cos(1.0e16), content="0.9576594805753825")
  
  // 接近零的值
  inspect(@math.sin(1.0e-10), content="1.0e-10")
  inspect(@math.cos(1.0e-10), content="1")
}

///|
test "math_complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 勾股定理验证
  let a = 3.0
  let b = 4.0
  let c = @math.hypotf(a, b)
  assert_true(c.is_close(5.0))
  
  // 三角恒等式验证
  let angle = @math.pi / 4.0
  let sin_val = @math.sin(angle)
  let cos_val = @math.cos(angle)
  let tan_val = @math.tan(angle)
  assert_true((sin_val * sin_val + cos_val * cos_val).is_close(1.0))
  assert_true((sin_val / cos_val).is_close(tan_val))
  
  // 对数恒等式验证
  let x = 10.0
  let log_exp = @math.log(@math.exp(x))
  let exp_log = @math.exp(@math.log(x))
  assert_true(log_exp.is_close(x))
  assert_true(exp_log.is_close(x))
  
  // 复合运算
  let result = @math.sqrt(@math.pow(3.0, 2.0) + @math.pow(4.0, 2.0))
  assert_true(result.is_close(5.0))
}