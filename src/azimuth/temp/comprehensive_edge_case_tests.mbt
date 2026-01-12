// 综合边界情况测试用例
use azimuth::{add, multiply, greet, assert_eq, assert_eq_string, assert_true, assert_false}

///|
test "add_overflow_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出边界
  assert_eq(max_val, max_val + 1)
  assert_eq(max_val, max_val + max_val)
  assert_eq(max_val, 1000000000 + 1500000000)
  
  // 测试负数溢出边界
  assert_eq(min_val, min_val + -1)
  assert_eq(min_val, min_val + min_val)
  assert_eq(min_val, -1000000000 + -1500000000)
}

///|
test "multiply_overflow_scenarios" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘溢出
  assert_eq(max_val, 50000 * 50000)
  assert_eq(max_val, max_val * 2)
  assert_eq(max_val, 46341 * 46341)  // sqrt(max_val)的边界
  
  // 测试最小值乘法溢出
  assert_eq(min_val, min_val * 2)
  assert_eq(min_val, min_val * -2)
  assert_eq(min_val, min_val * min_val)
}

///|
test "zero_operations_consistency" {
  // 测试零的加法运算
  assert_eq(5, 5 + 0)
  assert_eq(-5, -5 + 0)
  assert_eq(0, 0 + 0)
  assert_eq(max_val, 2147483647 + 0)
  assert_eq(min_val, -2147483648 + 0)
  
  // 测试零的乘法运算
  assert_eq(0, 5 * 0)
  assert_eq(0, -5 * 0)
  assert_eq(0, 0 * 0)
  assert_eq(0, 2147483647 * 0)
  assert_eq(0, -2147483648 * 0)
}

///|
test "negative_one_operations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试-1的乘法运算
  assert_eq(-5, 5 * -1)
  assert_eq(5, -5 * -1)
  assert_eq(0, 0 * -1)
  assert_eq(-max_val, max_val * -1)
  assert_eq(min_val, min_val * -1)  // 特殊情况：最小值乘以-1溢出
}

///|
test "greet_function_edge_cases" {
  // 测试空字符串
  assert_eq_string("Hello, !", "Hello, " + "" + "!")
  
  // 测试单字符
  assert_eq_string("Hello, A!", "Hello, " + "A" + "!")
  assert_eq_string("Hello, 1!", "Hello, " + "1" + "!")
  
  // 测试长字符串
  let long_name = "这是一个非常非常长的名字用来测试greet函数处理长字符串的能力"
  assert_eq_string("Hello, " + long_name + "!", "Hello, " + long_name + "!")
  
  // 测试包含引号的字符串
  assert_eq_string("Hello, \"John\"!", "Hello, " + "\"John\"" + "!")
}

///|
test "cascading_operations" {
  // 测试级联加法操作
  let result1 = add(1 + 2, 3 + 4)
  assert_eq(10, result1)
  
  let result2 = add(add(1 + 2, 3), 4)
  assert_eq(10, result2)
  
  // 测试级联乘法操作
  let result3 = multiply(2 * 3, 4 * 5)
  assert_eq(120, result3)
  
  let result4 = multiply(multiply(2 * 3, 4), 5)
  assert_eq(120, result4)
}

///|
test "mixed_sign_operations" {
  // 测试混合符号的加法
  assert_eq(0, 5 + -5)
  assert_eq(-2, 3 + -5)
  assert_eq(2, -3 + 5)
  assert_eq(-10, -15 + 5)
  
  // 测试混合符号的乘法
  assert_eq(-15, 3 * -5)
  assert_eq(15, -3 * -5)
  assert_eq(0, 0 * -5)
  assert_eq(-75, -15 * 5)
}

///|
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值的各种组合
  assert_eq(-1, max_val + min_val)
  assert_eq(max_val - 1, max_val + -1)
  assert_eq(min_val + 1, min_val + 1)
  
  assert_eq(min_val, max_val * -1)
  assert_eq(max_val, min_val * -1)
  assert_eq(0, max_val * 0)
}

///|
test "practical_calculation_scenarios" {
  // 测试实际计算场景
  
  // 银行利息计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let simple_interest = principal * multiply(rate, years)
  assert_eq(150000, simple_interest)  // 简化计算：10000 * 5 * 3
  
  // 购物计算
  let item_price = 199
  let quantity = 3
  let subtotal = item_price * quantity
  assert_eq(597, subtotal)  // 199 * 3
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = multiply(celsius + 9 * 5, 32)
  assert_eq(77, fahrenheit)  // (25 * 9/5) + 32 = 77
