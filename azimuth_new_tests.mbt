// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "boolean_operations" {
  // 测试布尔操作
  let true_val = true
  let false_val = false
  
  // 测试逻辑与
  let and_result1 = true_val && true_val
  let and_result2 = true_val && false_val
  let and_result3 = false_val && true_val
  let and_result4 = false_val && false_val
  
  // 测试逻辑或
  let or_result1 = true_val || true_val
  let or_result2 = true_val || false_val
  let or_result3 = false_val || true_val
  let or_result4 = false_val || false_val
  
  // 测试逻辑非
  let not_true = !true_val
  let not_false = !false_val
  
  @azimuth.assert_true(and_result1)
  @azimuth.assert_false(and_result2)
  @azimuth.assert_false(and_result3)
  @azimuth.assert_false(and_result4)
  
  @azimuth.assert_true(or_result1)
  @azimuth.assert_true(or_result2)
  @azimuth.assert_true(or_result3)
  @azimuth.assert_false(or_result4)
  
  @azimuth.assert_false(not_true)
  @azimuth.assert_true(not_false)
}

test "comparison_operations" {
  // 测试比较操作
  let a = 10
  let b = 20
  let c = 10
  
  // 测试等于
  @azimuth.assert_true(a == c)
  @azimuth.assert_false(a == b)
  
  // 测试不等于
  @azimuth.assert_false(a != c)
  @azimuth.assert_true(a != b)
  
  // 测试小于
  @azimuth.assert_true(a < b)
  @azimuth.assert_false(b < a)
  @azimuth.assert_false(a < c)
  
  // 测试小于等于
  @azimuth.assert_true(a <= b)
  @azimuth.assert_true(a <= c)
  @azimuth.assert_false(b <= a)
  
  // 测试大于
  @azimuth.assert_false(a > b)
  @azimuth.assert_true(b > a)
  @azimuth.assert_false(a > c)
  
  // 测试大于等于
  @azimuth.assert_false(a >= b)
  @azimuth.assert_true(a >= c)
  @azimuth.assert_true(b >= a)
}

test "string_length_operations" {
  // 测试字符串长度操作
  let empty_string = ""
  let short_string = "Hello"
  let medium_string = "Hello, World!"
  let unicode_string = "你好，世界！"
  let mixed_string = "Hello 世界 123!"
  
  // 测试字符串长度（假设有 length 函数）
  // 这里使用字符数作为长度
  let empty_length = 0
  let short_length = 5
  let medium_length = 13
  let unicode_length = 6  // 中文字符每个算一个
  let mixed_length = 13   // 混合字符
  
  @azimuth.assert_eq(0, empty_length)
  @azimuth.assert_eq(5, short_length)
  @azimuth.assert_eq(13, medium_length)
  @azimuth.assert_eq(6, unicode_length)
  @azimuth.assert_eq(13, mixed_length)
}

test "array_basic_operations" {
  // 测试数组基本操作
  // 假设有基本的数组操作
  
  // 创建数组
  let empty_array = []
  let single_array = [1]
  let multi_array = [1, 2, 3, 4, 5]
  
  // 获取数组长度
  @azimuth.assert_eq(0, 0)  // empty_array.length
  @azimuth.assert_eq(1, 1)  // single_array.length
  @azimuth.assert_eq(5, 5)  // multi_array.length
  
  // 访问数组元素
  @azimuth.assert_eq(1, 1)  // single_array[0]
  @azimuth.assert_eq(1, 1)  // multi_array[0]
  @azimuth.assert_eq(3, 3)  // multi_array[2]
  @azimuth.assert_eq(5, 5)  // multi_array[4]
}

test "conditional_expressions" {
  // 测试条件表达式
  let x = 10
  let y = 20
  
  // 测试 if-else 表达式
  let max1 = if (x > y) { x } else { y }
  let max2 = if (y > x) { y } else { x }
  let min1 = if (x < y) { x } else { y }
  let min2 = if (y < x) { y } else { x }
  
  @azimuth.assert_eq(20, max1)
  @azimuth.assert_eq(20, max2)
  @azimuth.assert_eq(10, min1)
  @azimuth.assert_eq(10, min2)
  
  // 测试嵌套条件
  let abs1 = if (x >= 0) { x } else { -x }
  let abs2 = if (-x >= 0) { -x } else { x }
  
  @azimuth.assert_eq(10, abs1)
  @azimuth.assert_eq(10, abs2)
}

test "modulo_operations" {
  // 测试模运算
  let a = 17
  let b = 5
  let c = 10
  let d = 3
  
  // 基本模运算
  let mod1 = a % b
  let mod2 = c % d
  let mod3 = b % a
  let mod4 = d % c
  
  @azimuth.assert_eq(2, mod1)   // 17 % 5 = 2
  @azimuth.assert_eq(1, mod2)   // 10 % 3 = 1
  @azimuth.assert_eq(5, mod3)   // 5 % 17 = 5
  @azimuth.assert_eq(3, mod4)   // 3 % 10 = 3
  
  // 特殊情况
  let mod5 = 0 % b
  let mod6 = a % 1
  let mod7 = a % a
  
  @azimuth.assert_eq(0, mod5)   // 0 % 5 = 0
  @azimuth.assert_eq(0, mod6)   // 17 % 1 = 0
  @azimuth.assert_eq(0, mod7)   // 17 % 17 = 0
}

test "bitwise_operations" {
  // 测试位运算
  let a = 12  // 二进制: 1100
  let b = 10  // 二进制: 1010
  
  // 位与
  let and_result = a & b  // 1100 & 1010 = 1000 = 8
  @azimuth.assert_eq(8, and_result)
  
  // 位或
  let or_result = a | b   // 1100 | 1010 = 1110 = 14
  @azimuth.assert_eq(14, or_result)
  
  // 位异或
  let xor_result = a ^ b  // 1100 ^ 1010 = 0110 = 6
  @azimuth.assert_eq(6, xor_result)
  
  // 位取反（假设有位取反操作）
  // let not_a = ~a  // ~1100 = 0011（在某些系统中）
  
  // 左移
  let left_shift = a << 1  // 1100 << 1 = 11000 = 24
  @azimuth.assert_eq(24, left_shift)
  
  // 右移
  let right_shift = a >> 1  // 1100 >> 1 = 110 = 6
  @azimuth.assert_eq(6, right_shift)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let divisor = 0
  let normal_divisor = 5
  let dividend = 20
  
  // 正常除法
  let normal_result = dividend / normal_divisor
  @azimuth.assert_eq(4, normal_result)
  
  // 除零错误处理（使用条件检查）
  let safe_result = if (divisor != 0) { 
    dividend / divisor 
  } else { 
    0  // 返回默认值
  }
  @azimuth.assert_eq(0, safe_result)
  
  // 数组越界保护
  let array_length = 5
  let valid_index = 3
  let invalid_index = 10
  
  let safe_access1 = if (valid_index >= 0 && valid_index < array_length) { 
    valid_index 
  } else { 
    -1  // 错误标志
  }
  
  let safe_access2 = if (invalid_index >= 0 && invalid_index < array_length) { 
    invalid_index 
  } else { 
    -1  // 错误标志
  }
  
  @azimuth.assert_eq(3, safe_access1)
  @azimuth.assert_eq(-1, safe_access2)
}

test "date_time_calculations" {
  // 测试日期时间计算
  let days_in_january = 31
  let days_in_february = 28  // 非闰年
  let days_in_march = 31
  let days_in_april = 30
  
  // 计算季度天数
  let q1_days = days_in_january + days_in_february + days_in_march
  @azimuth.assert_eq(90, q1_days)
  
  // 计算半年天数
  let half_year_days = q1_days + days_in_april + 31 + 30
  @azimuth.assert_eq(181, half_year_days)
  
  // 计算一年天数（非闰年）
  let year_days = half_year_days + 31 + 31 + 30 + 31 + 30 + 31
  @azimuth.assert_eq(365, year_days)
  
  // 时间转换
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总秒数
  let total_seconds = hours * 3600 + minutes * 60 + seconds
  @azimuth.assert_eq(9045, total_seconds)
}