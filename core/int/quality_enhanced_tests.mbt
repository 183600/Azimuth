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

// 高质量 MoonBit 测试用例 - Int 模块
// 包含10个全面的测试用例，覆盖核心功能和边界情况

///|
test "int_boundary_values_comprehensive" {
  // 测试整数边界值的全面处理
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 测试边界值运算
  inspect(@int.max_value - 1, content="2147483646")
  inspect(@int.min_value + 1, content="-2147483647")
  
  // 测试溢出行为
  inspect(@int.max_value + 1, content="-2147483648")
  inspect(@int.min_value - 1, content="2147483647")
}

///|
test "int_arithmetic_operations_edge_cases" {
  // 测试算术运算的边缘情况
  // 零值运算
  inspect(0 + 0, content="0")
  inspect(0 - 0, content="0")
  inspect(0 * 0, content="0")
  inspect(0 * @int.max_value, content="0")
  inspect(0 * @int.min_value, content="0")
  
  // 负数运算
  inspect(-5 + -3, content="-8")
  inspect(-5 - -3, content="-2")
  inspect(-5 * -3, content="15")
  inspect(-5 * 3, content="-15")
  
  // 混合符号运算
  inspect(-10 + 15, content="5")
  inspect(10 + -15, content="-5")
}

///|
test "int_division_modulo_comprehensive" {
  // 测试除法和取模的全面情况
  // 正数除法
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
  inspect(15 / 5, content="3")
  inspect(15 % 5, content="0")
  
  // 负数除法
  inspect(-10 / 3, content="-3")
  inspect(-10 % 3, content="-1")
  inspect(10 / -3, content="-3")
  inspect(10 % -3, content="1")
  inspect(-10 / -3, content="3")
  inspect(-10 % -3, content="-1")
  
  // 边界情况
  inspect(0 / 5, content="0")
  inspect(0 % 5, content="0")
  inspect(@int.max_value / 1, content="2147483647")
  inspect(@int.min_value / -1, content="2147483648")
}

///|
test "int_bit_operations_advanced" {
  // 测试高级位运算
  // 基本位运算
  inspect(0b1010 & 0b1100, content="8")
  inspect(0b1010 | 0b1100, content="14")
  inspect(0b1010 ^ 0b1100, content="6")
  inspect(~0b1010, content="-11")
  
  // 移位运算
  inspect(1 << 30, content="1073741824")
  inspect(1 << 31, content="-2147483648")
  inspect(-2147483648 >> 31, content="-1")
  inspect(1073741824 >> 1, content="536870912")
  
  // 位运算边界情况
  inspect(0 & @int.max_value, content="0")
  inspect(@int.max_value | 0, content="2147483647")
  inspect(~@int.max_value, content="-2147483648")
  inspect(~@int.min_value, content="2147483647")
}

///|
test "int_comparison_operations_logical" {
  // 测试比较运算和逻辑运算
  // 基本比较
  assert_eq(10 > 5, true)
  assert_eq(10 < 5, false)
  assert_eq(10 >= 10, true)
  assert_eq(10 <= 5, false)
  assert_eq(10 == 10, true)
  assert_eq(10 != 5, true)
  
  // 边界值比较
  assert_eq(@int.max_value > @int.min_value, true)
  assert_eq(@int.min_value < @int.max_value, true)
  assert_eq(@int.max_value >= @int.max_value, true)
  assert_eq(@int.min_value <= @int.min_value, true)
  
  // 逻辑组合
  assert_eq(10 > 5 && 10 < 20, true)
  assert_eq(10 > 5 && 10 > 20, false)
  assert_eq(10 > 5 || 10 > 20, true)
  assert_eq(10 < 5 || 10 > 20, false)
}

///|
test "int_type_conversions_precision" {
  // 测试类型转换的精度处理
  // 转换为其他整数类型
  inspect(100.to_int64(), content="100")
  inspect(-100.to_int64(), content="-100")
  inspect(@int.max_value.to_int64(), content="2147483647")
  inspect(@int.min_value.to_int64(), content="-2147483648")
  
  // 转换为无符号整数
  inspect(100.to_uint(), content="100")
  inspect(0.to_uint(), content="0")
  inspect((-1).to_uint(), content="4294967295")
  inspect(@int.min_value.to_uint(), content="2147483648")
  
  // 转换为浮点数
  inspect(100.to_double(), content="100.0")
  inspect(-100.to_double(), content="-100.0")
  inspect(@int.max_value.to_double(), content="2147483647.0")
  inspect(@int.min_value.to_double(), content="-2147483648.0")
}

///|
test "int_power_operations_efficiency" {
  // 测试幂运算的效率和正确性
  // 基本幂运算
  inspect(Int::pow(2, 0), content="1")
  inspect(Int::pow(2, 1), content="2")
  inspect(Int::pow(2, 10), content="1024")
  inspect(Int::pow(2, 15), content="32768")
  inspect(Int::pow(2, 20), content="1048576")
  
  // 不同基数的幂运算
  inspect(Int::pow(3, 3), content="27")
  inspect(Int::pow(5, 3), content="125")
  inspect(Int::pow(10, 3), content="1000")
  inspect(Int::pow(7, 4), content="2401")
  
  // 边界情况
  inspect(Int::pow(1, 100), content="1")
  inspect(Int::pow(0, 10), content="0")
  inspect(Int::pow(-2, 3), content="-8")
  inspect(Int::pow(-2, 4), content="16")
}

///|
test "int_hash_function_distribution" {
  // 测试哈希函数的分布特性
  // 测试连续整数的哈希值
  inspect(Hasher::new(seed=42)..combine(0).finalize(), content="-150257970")
  inspect(Hasher::new(seed=42)..combine(1).finalize(), content="-1768487279")
  inspect(Hasher::new(seed=42)..combine(2).finalize(), content="-1655999664")
  inspect(Hasher::new(seed=42)..combine(3).finalize(), content="-2063369419")
  inspect(Hasher::new(seed=42)..combine(4).finalize(), content="614052865")
  
  // 测试边界值的哈希值
  inspect(Hasher::new(seed=42)..combine(@int.max_value).finalize(), content="-1223216679")
  inspect(Hasher::new(seed=42)..combine(@int.min_value).finalize(), content="1228964332")
  
  // 测试负数的哈希值
  inspect(Hasher::new(seed=42)..combine(-100).finalize(), content="1918239266")
  inspect(Hasher::new(seed=42)..combine(-1000).finalize(), content="2106298229")
}

///|
test "int_byte_order_conversions" {
  // 测试字节序转换的正确性
  // 大端序转换
  let be_bytes = 0x12345678.to_be_bytes()
  inspect(be_bytes.length(), content="4")
  inspect(be_bytes[0].to_int(), content="18")
  inspect(be_bytes[1].to_int(), content="52")
  inspect(be_bytes[2].to_int(), content="86")
  inspect(be_bytes[3].to_int(), content="120")
  
  // 小端序转换
  let le_bytes = 0x12345678.to_le_bytes()
  inspect(le_bytes.length(), content="4")
  inspect(le_bytes[0].to_int(), content="120")
  inspect(le_bytes[1].to_int(), content="86")
  inspect(le_bytes[2].to_int(), content="52")
  inspect(le_bytes[3].to_int(), content="18")
  
  // 边界值测试
  let max_be_bytes = @int.max_value.to_be_bytes()
  inspect(max_be_bytes[0].to_int(), content="127")
  inspect(max_be_bytes[1].to_int(), content="255")
  inspect(max_be_bytes[2].to_int(), content="255")
  inspect(max_be_bytes[3].to_int(), content="255")
}

///|
test "int_complex_arithmetic_scenarios" {
  // 测试复杂算术场景
  // 运算符优先级测试
  inspect(2 + 3 * 4, content="14")
  inspect((2 + 3) * 4, content="20")
  inspect(10 - 2 * 3, content="4")
  inspect(10 + 20 / 5, content="14")
  inspect(10 * 2 + 3, content="23")
  inspect(10 * (2 + 3), content="50")
  
  // 混合运算测试
  inspect(5 + 3 << 2, content="32")
  inspect(5 + (3 << 2), content="17")
  inspect(10 & 5 | 3, content="3")
  inspect(10 & (5 | 3), content="0")
  
  // 复杂表达式
  inspect((10 + 5) * (3 - 1) + 8 / 4, content="32")
  inspect(100 - 25 * 3 + 50 / 10, content="30")
  inspect((1 << 10) - (1 << 5), content="992")
}