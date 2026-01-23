// 新增的 MoonBit 标准测试用例
// 针对 azimuth 库的核心功能进行补充测试
// 包含10个测试用例

test "add_function_large_numbers" {
  // 测试加法函数处理大数
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(0, add(1000000, -1000000))
}

test "multiply_function_with_zero" {
  // 测试乘法函数与零的特殊情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(-12345, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 999))
  assert_eq(0, divide_with_ceil(0, -999))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base = 10
  let multiplier = 3
  let added = 7
  let divisor = 4
  
  let result = divide_with_ceil(add(multiply(base, multiplier), added), divisor)
  assert_eq(10, result)  // ceil((10*3+7)/4) = ceil(37/4) = 10
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 5*(3+2) = 5*3 + 5*2 = 25
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_items = 47
  let containers_needed = divide_with_ceil(total_items, 10)
  assert_eq(5, containers_needed)  // ceil(47/10) = 5
  
  let total_capacity = multiply(containers_needed, 10)
  assert_eq(50, total_capacity)  // 5 * 10 = 50
}

test "string_input_validation" {
  // 测试字符串输入验证
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, space separated!", greet("space separated"))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-15, add(-8, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 4))
}

test "identity_properties" {
  // 测试恒等性质
  assert_eq(42, add(42, 0))  // 加法恒等元
  assert_eq(42, multiply(42, 1))  // 乘法恒等元
  assert_eq(-42, multiply(42, -1))  // 乘法负恒等元
  assert_eq(1, divide_with_ceil(42, 42))  // 自除恒等
}