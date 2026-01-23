// 标准 MoonBit 综合测试用例
// 测试 azimuth 库的核心功能和边界情况

test "add_function_comprehensive" {
  // 测试加法函数的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(-12345, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🚀🌟!", greet("🚀🌟"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(5, divide_with_ceil(13, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-2, divide_with_ceil(8, -3))
  assert_eq(2, divide_with_ceil(-8, -4))
  assert_eq(-1, divide_with_ceil(-3, 5))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let sum = add(b, c)
  let left_side = multiply(a, sum)
  
  let product1 = multiply(a, b)
  let product2 = multiply(a, c)
  let right_side = add(product1, product2)
  
  assert_eq(left_side, right_side)
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  // 计算订单总价（商品价格 × 数量 + 运费）
  let unit_price = 89
  let quantity = 3
  let shipping = 15
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(282, total)
  
  // 计算需要的包装箱数量
  let total_products = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  assert_eq(4, boxes_needed)
}

test "string_operations_edge_cases" {
  // 测试字符串操作的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABCdef123!", greet("ABCdef123"))
  assert_eq_string("Hello, !@#$%^&*()!", greet("!@#$%^&*()"))
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：计算项目总工时和所需人员数量
  let tasks = 5
  let hours_per_task = 8
  let total_hours = multiply(tasks, hours_per_task)
  
  let working_hours_per_day = 8
  let days_needed = divide_with_ceil(total_hours, working_hours_per_day)
  
  let buffer_days = 2
  let total_days = add(days_needed, buffer_days)
  
  assert_eq(7, total_days)
}

test "mathematical_identities_verification" {
  // 测试数学恒等式验证
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(add(x, y), add(y, x))
  assert_eq(x, add(x, 0))
  
  // 乘法恒等式
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  
  // 分配律验证
  let z = 3
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
}