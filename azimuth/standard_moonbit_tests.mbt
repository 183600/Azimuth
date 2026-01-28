// 标准 MoonBit 测试用例
// 为 azimuth 项目创建的高质量测试用例

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(10, subtract(0, -10))
}

test "boolean_assertions" {
  // 测试布尔断言功能
  assert_true(5 > 3)
  assert_true(10 == 10)
  assert_false(3 > 5)
  assert_false(5 == 3)
}

test "mathematical_associative_law" {
  // 测试数学结合律
  let a = 5
  let b = 8
  let c = 12
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "boundary_value_testing" {
  // 测试边界值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, subtract(5, 5))
}

test "real_world_shopping_cart" {
  // 测试实际购物车场景
  let item_price = 25
  let quantity = 4
  let discount = 15
  let tax = 8
  
  let subtotal = multiply(item_price, quantity)
  let after_discount = subtract(subtotal, discount)
  let total = add(after_discount, tax)
  
  assert_eq(100, subtotal)
  assert_eq(85, after_discount)
  assert_eq(93, total)
}

test "string_processing_with_greet" {
  // 测试字符串处理与问候功能
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, !", greet(""))
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 除零安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, subtract(0, 2000000))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简单利息计算: principal + (principal * rate * years) / 100
  let interest = divide_with_ceil(multiply(multiply(principal, rate), years), 100)
  let total = add(principal, interest)
  
  assert_eq(150, interest)
  assert_eq(1150, total)
}

test "performance_related_operations" {
  // 测试性能相关操作
  let base = 100
  let iterations = 1000
  
  // 模拟循环累加
  let result = multiply(base, iterations)
  
  // 模拟批量处理
  let batch_size = 50
  let batches = divide_with_ceil(iterations, batch_size)
  
  assert_eq(100000, result)
  assert_eq(20, batches)
}

test "data_type_consistency" {
  // 测试数据类型一致性
  // 整数运算
  assert_eq(10, add(5, 5))
  assert_eq(25, multiply(5, 5))
  assert_eq(0, subtract(5, 5))
  
  // 字符串运算
  assert_eq_string("Hello, 5!", greet("5"))
  assert_eq_string("Hello, true!", greet("true"))
  assert_eq_string("Hello, false!", greet("false"))
}