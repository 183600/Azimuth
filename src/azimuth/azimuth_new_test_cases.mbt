// 新增的 MoonBit 测试用例
// 测试 azimuth 包的核心功能和边界情况

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(11, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(4, divide_with_ceil(13, 4))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-2, divide_with_ceil(-8, 4))
}

test "divide_with_ceil_zero" {
  // 测试包含0的向上取整除法
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "add_chained_operations" {
  // 测试链式加法运算
  let result = add(add(1, 2), add(3, 4))
  assert_eq(10, result)
  
  let result2 = add(add(100, 200), add(300, 400))
  assert_eq(1000, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "greet_unicode_variations" {
  // 测试各种 Unicode 字符串
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "extreme_boundary_combinations" {
  // 测试极值边界组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, add(max_val, 0))
  
  // 边界值附近的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景
  let principal = 10000
  let rate = 5
  let time = 3
  let fee = 100
  
  // 复利计算: principal * (1 + rate * time)
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  let final_amount = add(total, fee)
  
  assert_eq(161000, final_amount)
}

test "mathematical_series_validation" {
  // 测试数学级数验证
  // 验证等差数列求和: n * (first + last) / 2
  let n = 10
  let first = 1
  let last = 10
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(55, sum)
  
  // 验证等比数列求和(简化版)
  let ratio = 2
  let terms = 4
  let geo_sum = add(add(add(1, 2), 4), 8)
  assert_eq(15, geo_sum)
}