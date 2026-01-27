// 标准 MoonBit 测试套件 - 为 azimuth 库添加10个高质量测试用例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_commutativity" {
  // 测试基本算术交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_precision_handling" {
  // 测试向上取整除法的精度处理
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 5))  // 0/5 = 0
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  assert_eq(x, add(x, 0))  // 加法单位元
  assert_eq(x, multiply(x, 1))  // 乘法单位元
  assert_eq(0, multiply(x, 0))  // 零乘积性质
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算订单总价（商品价格 × 数量 + 运费）
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping_cost)
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-7, 11))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  assert_eq(34, resources_per_team)  // ceil(100/3) = 34
  // 验证总分配量
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(102, total_allocated)
}

test "edge_case_boundary_values" {
  // 测试边界值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(5, 0))  // 除零安全处理
  assert_eq(0, divide_with_ceil(-5, 0))  // 除零安全处理
}

test "string_processing_with_special_characters" {
  // 测试特殊字符的字符串处理
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, email@domain.com!", greet("email@domain.com"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  let input_size = 1000
  // 模拟 O(n) 算法
  let linear_complexity = multiply(input_size, 2)
  // 模拟 O(log n) 算法（简化为除法）
  let log_complexity = divide_with_ceil(input_size, 100)
  assert_eq(2000, linear_complexity)
  assert_eq(10, log_complexity)
}