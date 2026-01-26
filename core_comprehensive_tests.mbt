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

// 综合核心测试用例 - 为 MoonBit 标准库添加高质量测试覆盖

// 辅助断言函数
pub fn assert_eq_int(expected : Int, actual : Int) -> Unit {
  if (expected != actual) {
    @builtin.abort("Int assertion failed")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected != actual) {
    @builtin.abort("String assertion failed")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (!condition) {
    @builtin.abort("Expected true")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Expected false")
  }
}

// 测试用例 1: 整数运算边界测试
test "integer_arithmetic_boundary" {
  // 测试整数边界值运算
  assert_eq_int(@int.max_value, 2147483647)
  assert_eq_int(@int.min_value, -2147483648)
  
  // 测试溢出行为
  assert_eq_int(@int.max_value + 1, @int.min_value)
  assert_eq_int(@int.min_value - 1, @int.max_value)
  
  // 测试绝对值函数
  assert_eq_int(Int::abs(42), 42)
  assert_eq_int(Int::abs(-42), 42)
  assert_eq_int(Int::abs(0), 0)
}

// 测试用例 2: 字符串操作综合测试
test "string_operations_comprehensive" {
  // 测试字符串连接
  let s1 = "Hello"
  let s2 = "MoonBit"
  let result = s1 + ", " + s2 + "!"
  assert_eq_string("Hello, MoonBit!", result)
  
  // 测试空字符串
  assert_eq_string("", String::default())
  
  // 测试 Unicode 字符串
  let unicode_str = "测试中文 🚀"
  assert_eq_string("测试中文 🚀", unicode_str)
  
  // 测试字符串长度计算
  assert_eq_int(8, "Hello, 世界".length())
}

// 测试用例 3: 数学函数核心测试
test "mathematical_functions_core" {
  // 测试基本数学运算
  assert_eq_int(15, 10 + 5)
  assert_eq_int(5, 10 - 5)
  assert_eq_int(50, 10 * 5)
  assert_eq_int(2, 10 / 4)
  assert_eq_int(2, 10 % 4)
  
  // 测试位运算
  assert_eq_int(8, 12 & 10)  // 1100 & 1010 = 1000
  assert_eq_int(14, 12 | 10) // 1100 | 1010 = 1110
  assert_eq_int(6, 12 ^ 10)  // 1100 ^ 1010 = 0110
  assert_eq_int(40, 10 << 2)
  assert_eq_int(2, 10 >> 2)
}

// 测试用例 4: 数组操作基础测试
test "array_operations_basic" {
  // 测试数组创建和访问
  let arr = [1, 2, 3, 4, 5]
  assert_eq_int(3, arr[2])
  assert_eq_int(5, arr.length())
  
  // 测试数组边界
  let empty_arr : Array[Int] = []
  assert_eq_int(0, empty_arr.length())
  
  // 测试数组比较
  let arr2 = [1, 2, 3, 4, 5]
  assert_true(arr == arr2)
}

// 测试用例 5: 布尔逻辑和条件测试
test "boolean_logic_conditions" {
  // 测试基本布尔值
  assert_true(true)
  assert_false(false)
  
  // 测试布尔运算
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
  assert_true(!false)
  
  // 测试比较运算
  assert_true(5 > 3)
  assert_true(5 >= 5)
  assert_true(3 < 5)
  assert_true(5 <= 5)
  assert_true(5 == 5)
  assert_true(5 != 3)
}

// 测试用例 6: 类型转换测试
test "type_conversion_operations" {
  // 测试整数到字节转换
  let value = 0x12345678
  let bytes_be = value.to_be_bytes()
  let bytes_le = value.to_le_bytes()
  
  // 验证字节顺序
  assert_eq_int(4, bytes_be.length())
  assert_eq_int(4, bytes_le.length())
  
  // 测试字节到整数转换
  let reconstructed = @int.from_be_bytes(bytes_be)
  assert_eq_int(value, reconstructed)
}

// 测试用例 7: 错误处理和边界条件测试
test "error_handling_boundaries" {
  // 测试除零错误处理
  assert_eq_int(0, 10 / 0)  // MoonBit 的除零行为
  
  // 测试极值运算
  let max_val = @int.max_value
  let min_val = @int.min_value
  
  // 测试溢出处理
  assert_eq_int(min_val, max_val + 1)
  assert_eq_int(max_val, min_val - 1)
  
  // 测试空值处理
  let empty_str = ""
  assert_eq_int(0, empty_str.length())
}

// 测试用例 8: 哈希和比较测试
test "hashing_and_equality" {
  // 测试整数哈希
  let hasher = @hash.Hasher::new(seed=42)
  let hash1 = hasher..combine(123).finalize()
  let hash2 = hasher..combine(456).finalize()
  
  // 验证不同数字产生不同哈希
  assert_true(hash1 != hash2)
  
  // 测试相同数字产生相同哈希
  let hasher2 = @hash.Hasher::new(seed=42)
  let hash3 = hasher2..combine(123).finalize()
  assert_eq_int(hash1, hash3)
}

// 测试用例 9: 性能和效率测试
test "performance_efficiency" {
  // 测试大数运算性能
  let large_num = 1000000
  let result = large_num * large_num
  assert_eq_int(1000000000000, result)
  
  // 测试循环效率
  let mut sum = 0
  for i = 0; i < 1000; i = i + 1 {
    sum = sum + i
  }
  assert_eq_int(499500, sum)  // 0+1+2+...+999 = 499500
}

// 测试用例 10: 实际应用场景测试
test "real_world_application" {
  // 模拟实际计算场景：计算圆的面积和周长
  let radius = 5
  let pi_approx = 3.14159  // 使用近似值
  
  // 计算面积 (π * r²)
  let area = pi_approx * radius * radius
  assert_true(area > 78.0 && area < 79.0)  // 约 78.54
  
  // 计算周长 (2 * π * r)
  let circumference = 2 * pi_approx * radius
  assert_true(circumference > 31.0 && circumference < 32.0)  // 约 31.42
  
  // 模拟数据处理场景
  let data = [10, 20, 30, 40, 50]
  let mut average = 0
  for value in data {
    average = average + value
  }
  average = average / data.length()
  assert_eq_int(30, average)  // (10+20+30+40+50)/5 = 30
}