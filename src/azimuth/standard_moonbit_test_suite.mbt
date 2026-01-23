// 标准 MoonBit 测试套件
// 包含针对 azimuth 库核心功能的高质量测试用例

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 10))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(16, multiply(-4, -4))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_business_scenario" {
  // 实际业务场景：计算订单总价
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "complex_calculations_nested" {
  // 复杂嵌套计算
  let result = add(
    multiply(10, divide_with_ceil(25, 4)),
    multiply(5, divide_with_ceil(17, 3))
  )
  assert_eq(85, result)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算测试
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
}