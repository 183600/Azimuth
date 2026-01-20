// Azimuth 项目额外测试用例
// 测试 lib.mbt 中定义的各种函数

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, divide_with_ceil(20, 4))
}

test "divide_with_ceil_with_remainder" {
  // 测试有余数的向上取整除法
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(7, divide_with_ceil(40, 6))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(11, divide_with_ceil(100, 9))
}

test "divide_with_ceil_edge_cases" {
  // 测试边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
  assert_eq(-4, divide_with_ceil(-15, 4))
}

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  let a = 12
  let b = 5
  let c = 3
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let product = multiply(sum, c)
  assert_eq(51, product)
  
  // 计算 a * b + c
  let product2 = multiply(a, b)
  let sum2 = add(product2, c)
  assert_eq(63, sum2)
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  assert_eq_string("Hello, MoonBit@2024!", greet("MoonBit@2024"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "overflow_protection_comprehensive" {
  // 综合溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, add(max_val, 1000000))
  assert_eq(min_val, add(min_val, -1000000))
  assert_eq(max_val, add(max_val/2, max_val/2 + 1))
  
  // 乘法溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
}

test "mathematical_series_and_patterns" {
  // 数学序列和模式测试
  // 计算等差数列 2, 5, 8, 11, 14 的和
  let sum = add(add(add(add(2, 5), 8), 11), 14)
  assert_eq(40, sum)
  
  // 计算等比数列 2, 4, 8, 16 的积
  let product = multiply(multiply(2, 4), multiply(8, 16))
  assert_eq(1024, product)
  
  // 验证平方和公式：1² + 2² + 3² + 4² = 30
  let square_sum = add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4))
  assert_eq(30, square_sum)
}

test "practical_business_calculations" {
  // 实际业务计算测试
  // 计算折扣价格：原价300，折扣20%，最终价格
  let original_price = 300
  let discount_rate = 20
  let discount_amount = multiply(original_price, discount_rate) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(240, final_price)
  
  // 计算复利：本金1000，利率5%，3年
  let principal = 1000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(100, multiply(rate, years)))
  assert_eq(115000, total)
}

test "assert_functions_various_types" {
  // 测试各种类型的断言函数
  // 整数断言
  assert_eq(42, add(20, 22))
  assert_eq(-10, add(5, -15))
  
  // 字符串断言
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  
  // 布尔断言
  assert_true(add(1, 1) == 2)
  assert_false(add(1, 1) == 3)
  assert_true(multiply(6, 7) == 42)
  assert_false(multiply(6, 7) == 43)
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  // 计算 ((a + b) * (c - d)) / e 的向上取整
  let a = 15
  let b = 10
  let c = 20
  let d = 8
  let e = 3
  
  let sum = add(a, b)  // 25
  let diff = add(c, -d)  // 12
  let product = multiply(sum, diff)  // 300
  let result = divide_with_ceil(product, e)  // 100
  
  assert_eq(100, result)
  
  // 计算平均值的向上取整
  let numbers_sum = add(add(add(13, 17), 19), 11)  // 60
  let count = 4
  let average_ceil = divide_with_ceil(numbers_sum, count)
  assert_eq(15, average_ceil)
}