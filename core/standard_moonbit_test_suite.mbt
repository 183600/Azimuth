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

// 标准 MoonBit 测试套件 - 包含10个高质量测试用例
// 涵盖 int、string、math 等核心模块的功能测试

///|
test "int arithmetic edge cases" {
  // 测试整数算术的边缘情况
  let max_int = @int.max_value
  let min_int = @int.min_value
  
  // 测试溢出行为
  assert_eq(max_int + 1, min_int)
  assert_eq(min_int - 1, max_int)
  
  // 测试零的特殊性质
  assert_eq(0 * max_int, 0)
  assert_eq(0 * min_int, 0)
  assert_eq(max_int + 0, max_int)
  assert_eq(min_int + 0, min_int)
  
  // 测试除法的边界情况
  assert_eq(1 / 1, 1)
  assert_eq(-1 / 1, -1)
  assert_eq(1 / -1, -1)
  assert_eq(-1 / -1, 1)
}

///|
test "string manipulation operations" {
  // 测试字符串操作功能
  let base = "MoonBit"
  let prefix = "Hello "
  let suffix = " 2024"
  
  // 测试字符串连接
  let greeting = prefix + base
  assert_eq(greeting, "Hello MoonBit")
  
  let full_message = greeting + suffix
  assert_eq(full_message, "Hello MoonBit 2024")
  
  // 测试空字符串操作
  let empty = ""
  assert_eq(empty + base, base)
  assert_eq(base + empty, base)
  assert_eq(empty + empty, empty)
  
  // 测试长度计算
  assert_eq(base.length(), 7)
  assert_eq(greeting.length(), 12)
  assert_eq(full_message.length(), 17)
  assert_eq(empty.length(), 0)
}

///|
test "math floating point precision" {
  // 测试浮点数精度和特殊值
  let pi = @math.pi
  let e = @math.e
  
  // 测试基本常数精度
  assert_eq(@math.floorf(pi), 3)
  assert_eq(@math.ceilf(pi), 4)
  assert_eq(@math.roundf(pi), 3)
  
  assert_eq(@math.floorf(e), 2)
  assert_eq(@math.ceilf(e), 3)
  assert_eq(@math.roundf(e), 3)
  
  // 测试平方根精度
  assert_eq(@math.sqrtf(0), 0)
  assert_eq(@math.sqrtf(1), 1)
  assert_eq(@math.sqrtf(4), 2)
  assert_eq(@math.sqrtf(9), 3)
  assert_eq(@math.sqrtf(16), 4)
  
  // 测试绝对值函数
  assert_eq(@math.fabsf(3.14), 3.14)
  assert_eq(@math.fabsf(-3.14), 3.14)
  assert_eq(@math.fabsf(0.0), 0.0)
}

///|
test "math trigonometric identities" {
  // 测试三角函数恒等式
  let zero = 0.0
  let pi = @math.pi
  let pi_over_2 = pi / 2.0
  let pi_over_4 = pi / 4.0
  
  // 测试基本三角函数值
  assert_eq(@math.sinf(zero), 0.0)
  assert_eq(@math.cosf(zero), 1.0)
  assert_eq(@math.tanf(zero), 0.0)
  
  // 测试特殊角度的值
  assert_eq(@math.sinf(pi_over_2), 1.0)
  assert_eq(@math.cosf(pi), -1.0)
  
  // 测试勾股定理
  let sin_pi_over_4 = @math.sinf(pi_over_4)
  let cos_pi_over_4 = @math.cosf(pi_over_4)
  let sin_squared_plus_cos_squared = sin_pi_over_4 * sin_pi_over_4 + cos_pi_over_4 * cos_pi_over_4
  assert_eq(@math.roundf(sin_squared_plus_cos_squared * 1000) / 1000, 1.0)
}

///|
test "int bit manipulation advanced" {
  // 测试高级位操作
  let a = 0b10101010  // 170
  let b = 0b01010101  // 85
  
  // 测试基本位运算
  assert_eq(a & b, 0)          // 10101010 & 01010101 = 00000000
  assert_eq(a | b, 255)        // 10101010 | 01010101 = 11111111
  assert_eq(a ^ b, 255)        // 10101010 ^ 01010101 = 11111111
  
  // 测试位移操作
  assert_eq(a << 1, 340)       // 10101010 << 1 = 101010100
  assert_eq(a >> 1, 85)        // 10101010 >> 1 = 01010101
  assert_eq(b << 2, 340)       // 01010101 << 2 = 0101010100
  assert_eq(b >> 2, 21)        // 01010101 >> 2 = 00010101
  
  // 测试位移的边界情况
  assert_eq(0 << 10, 0)
  assert_eq(1 << 0, 1)
  assert_eq(1 >> 0, 1)
}

///|
test "string unicode and special characters" {
  // 测试 Unicode 和特殊字符处理
  let chinese = "中文测试"
  let emoji = "🚀🌟💻"
  let mixed = "Hello 世界! 🌍"
  let whitespace = "  \t\n\r  "
  
  // 测试 Unicode 字符串长度
  assert_eq(chinese.length(), 4)
  assert_eq(emoji.length(), 3)
  assert_eq(mixed.length(), 9)
  assert_eq(whitespace.length(), 6)
  
  // 测试特殊字符连接
  let combined = chinese + " " + emoji
  assert_eq(combined, "中文测试 🚀🌟💻")
  
  // 测试空格和制表符
  let with_tabs = "Col1\tCol2\tCol3"
  assert_eq(with_tabs.length(), 13)
  
  let with_newlines = "Line1\nLine2\nLine3"
  assert_eq(with_newlines.length(), 17)
}

///|
test "math power and logarithm relationships" {
  // 测试幂和对数函数的关系
  let base = 2.0
  let exponent = 3.0
  
  // 测试幂函数
  let power_result = @math.powf(base, exponent)
  assert_eq(@math.roundf(power_result), 8.0)
  
  // 测试对数函数的逆关系
  let log_result = @math.logf(@math.expf(1.0))
  assert_eq(@math.roundf(log_result * 1000) / 1000, 1.0)
  
  // 测试以 2 为底的对数
  let log2_result = @math.log2f(8.0)
  assert_eq(@math.roundf(log2_result), 3.0)
  
  // 测试以 10 为底的对数
  let log10_result = @math.log10f(100.0)
  assert_eq(@math.roundf(log10_result), 2.0)
  
  // 测试立方根
  assert_eq(@math.cbrtf(27.0), 3.0)
  assert_eq(@math.cbrtf(-27.0), -3.0)
}

///|
test "complex arithmetic expressions" {
  // 测试复杂算术表达式
  let x = 5
  let y = 3
  let z = 2
  
  // 测试运算优先级
  assert_eq(x + y * z, 11)     // 5 + (3 * 2) = 11
  assert_eq((x + y) * z, 16)   // (5 + 3) * 2 = 16
  assert_eq(x - y - z, 0)      // (5 - 3) - 2 = 0
  assert_eq(x - (y - z), 4)    // 5 - (3 - 2) = 4
  
  // 测试混合运算
  assert_eq(x * y + x * z, 25) // 5*3 + 5*2 = 15 + 10 = 25
  assert_eq(x * (y + z), 25)   // 5 * (3 + 2) = 5 * 5 = 25
  
  // 测试除法和取模
  assert_eq(17 / 3, 5)
  assert_eq(17 % 3, 2)
  assert_eq(-17 / 3, -5)
  assert_eq(-17 % 3, -2)
}

///|
test "string pattern matching scenarios" {
  // 测试字符串模式匹配场景
  let email = "user@example.com"
  let phone = "123-456-7890"
  let url = "https://www.moonbitlang.com"
  
  // 测试包含特殊字符的字符串
  assert_eq(email.contains("@"), true)
  assert_eq(email.contains("."), true)
  assert_eq(phone.contains("-"), true)
  assert_eq(url.contains("://"), true)
  
  // 测试字符串分割概念（通过索引模拟）
  let domain_start = email.index_of("@") + 1
  assert_eq(domain_start > 0, true)
  
  // 测试字符串前缀和后缀概念
  let starts_with_http = url.length() >= 5 and url[0] == 'h' and url[1] == 't' and url[2] == 't' and url[3] == 'p'
  assert_eq(starts_with_http, true)
  
  // 测试数字字符串
  let number_str = "12345"
  assert_eq(number_str.length(), 5)
}

///|
test "math special value handling" {
  // 测试数学特殊值处理
  let very_large = 1000000.0
  let very_small = 0.000001
  let normal = 42.0
  
  // 测试大数运算
  let large_sqrt = @math.sqrtf(very_large)
  assert_eq(@math.roundf(large_sqrt), 1000.0)
  
  // 测试小数运算
  let small_sqrt = @math.sqrtf(very_small)
  assert_eq(small_sqrt > 0.0, true)
  
  // 测试双曲函数
  assert_eq(@math.sinhf(0.0), 0.0)
  assert_eq(@math.coshf(0.0), 1.0)
  assert_eq(@math.tanhf(0.0), 0.0)
  
  // 测试最小值和最大值函数
  assert_eq(@math.fminf(3.14, 2.71), 2.71)
  assert_eq(@math.fmaxf(3.14, 2.71), 3.14)
  assert_eq(@math.fminf(-1.0, 1.0), -1.0)
  assert_eq(@math.fmaxf(-1.0, 1.0), 1.0)
  
  // 测试正差函数
  assert_eq(@math.fdimf(5.0, 3.0), 2.0)
  assert_eq(@math.fdimf(3.0, 5.0), 0.0)
}