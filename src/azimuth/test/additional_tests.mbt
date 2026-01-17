// 额外的 MoonBit 测试用例
// 导入主模块
use azimuth

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(11, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(4, divide_with_ceil(13, 4))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(6, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-1, divide_with_ceil(-5, 5))
  assert_eq(-1, divide_with_ceil(-4, 5))
  assert_eq(0, divide_with_ceil(-1, 5))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列：斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
}

test "geometric_progression" {
  // 测试几何级数：2, 6, 18, 54, ...
  // a(n) = a(1) * r^(n-1)，其中 r = 3
  let a1 = 2
  let ratio = 3
  let a2 = multiply(a1, ratio)      // 6
  let a3 = multiply(a2, ratio)      // 18
  let a4 = multiply(a3, ratio)      // 54
  
  assert_eq(6, a2)
  assert_eq(18, a3)
  assert_eq(54, a4)
  
  // 计算前4项的和
  let sum = add(add(add(a1, a2), a3), a4)
  assert_eq(80, sum)
}

test "string_concatenation_with_numbers" {
  // 测试字符串与数字的组合
  let age = 25
  let name = "Alice"
  let age_str = "25"
  
  // 通过字符串拼接创建个人信息
  let greeting = greet(name)
  let age_info = "Age: " + age_str
  let full_info = greeting + " " + age_info
  
  assert_eq_string("Hello, Alice! Age: 25", full_info)
}

test "calculator_simulation" {
  // 模拟简单计算器运算
  let x = 15
  let y = 4
  
  // 加法
  let sum = add(x, y)
  assert_eq(19, sum)
  
  // 乘法
  let product = multiply(x, y)
  assert_eq(60, product)
  
  // 模拟减法：x - y = x + (-y)
  let difference = add(x, -y)
  assert_eq(11, difference)
  
  // 模拟除法：x / y（整数除法）
  let quotient = x / y
  assert_eq(3, quotient)
  
  // 模拟取余：x % y
  let remainder = x % y
  assert_eq(3, remainder)
}

test "boolean_logic_simulation" {
  // 使用数值模拟布尔逻辑运算
  // 真 = 1, 假 = 0
  let true_val = 1
  let false_val = 0
  
  // 逻辑与：a && b = min(a, b)
  let and_true = min_val(true_val, true_val)
  let and_false1 = min_val(true_val, false_val)
  let and_false2 = min_val(false_val, false_val)
  
  assert_eq(1, and_true)
  assert_eq(0, and_false1)
  assert_eq(0, and_false2)
  
  // 逻辑或：a || b = max(a, b)
  let or_true1 = max_val(true_val, true_val)
  let or_true2 = max_val(true_val, false_val)
  let or_false = max_val(false_val, false_val)
  
  assert_eq(1, or_true1)
  assert_eq(1, or_true2)
  assert_eq(0, or_false)
  
  // 逻辑非：!a = 1 - a
  let not_true = add(1, -true_val)
  let not_false = add(1, -false_val)
  
  assert_eq(0, not_true)
  assert_eq(1, not_false)
}

test "array_sum_simulation" {
  // 模拟数组求和：sum = a1 + a2 + a3 + a4 + a5
  let a1 = 10
  let a2 = 20
  let a3 = 30
  let a4 = 40
  let a5 = 50
  
  let sum1 = add(a1, a2)
  let sum2 = add(sum1, a3)
  let sum3 = add(sum2, a4)
  let total_sum = add(sum3, a5)
  
  assert_eq(150, total_sum)
  
  // 计算平均值
  let count = 5
  let average = total_sum / count
  assert_eq(30, average)
}

test "temperature_conversion_precise" {
  // 精确的温度转换：摄氏度转华氏度
  // 公式：F = C × 9/5 + 32
  // 使用整数运算：F = (C × 9 + 160) / 5
  
  let celsius = 25
  let numerator = add(multiply(celsius, 9), 160)
  let fahrenheit = numerator / 5
  
  assert_eq(77, fahrenheit)  // (25 × 9 + 160) / 5 = 385 / 5 = 77
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 9), 160) / 5
  assert_eq(32, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 9), 160) / 5
  assert_eq(212, boiling_f)
}