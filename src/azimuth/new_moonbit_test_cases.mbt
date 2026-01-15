// 新增的 MoonBit 测试用例
// 测试项目的核心功能：add, multiply, greet, divide_with_ceil 等函数

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(2, -3))
  assert_eq(100, add(50, 50))
}

test "add_function_overflow_handling" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(25, multiply(5, 5))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1和-1相乘
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出处理
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_functionality" {
  // 测试 divide_with_ceil 函数的功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(5, divide_with_ceil(11, 2))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算表达式：(a + b) * c - d / e
  let a = 10
  let b = 5
  let c = 3
  let d = 12
  let e = 4
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let division = d / e
  let result = add(product, -division)
  
  assert_eq(43, result)  // (10 + 5) * 3 - 12 / 4 = 45 - 3 = 42
}

test "real_world_application_test" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  
  // 应用折扣
  let after_discount = add(subtotal, -discount)
  
  // 计算税费
  let tax = multiply(after_discount, tax_rate) / 100
  let final_total = add(after_discount, tax)
  
  assert_eq(637, final_total)  // (199*2 + 299*1 - 50) + 8% = 647 + 51 = 698
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 验证加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 验证乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证分配律：a * (b + c) = a * b + a * c
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "assert_functions_comprehensive_test" {
  // 测试断言函数的综合使用
  // 测试 assert_true 和 assert_false
  assert_true(5 > 3)
  assert_false(5 < 3)
  
  // 测试 assert_eq
  assert_eq(42, multiply(6, 7))
  assert_eq(100, add(50, 50))
  
  // 测试 assert_eq_string
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Test!", greet("Test"))
}