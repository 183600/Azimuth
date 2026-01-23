// 标准 MoonBit 综合测试用例
// 测试 azimuth 库的核心功能和边界情况

test "add_function_comprehensive" {
  // 测试加法函数的各种情况
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(2, @azimuth.add(-5, 7))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, José!", @azimuth.greet("José"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🚀🌟!", @azimuth.greet("🚀🌟"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(8, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-8, -4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 5))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let sum = @azimuth.add(b, c)
  let left_side = @azimuth.multiply(a, sum)
  
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(a, c)
  let right_side = @azimuth.add(product1, product2)
  
  @azimuth.assert_eq(left_side, right_side)
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  // 计算订单总价（商品价格 × 数量 + 运费）
  let unit_price = 89
  let quantity = 3
  let shipping = 15
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(282, total)
  
  // 计算需要的包装箱数量
  let total_products = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_products, capacity_per_box)
  @azimuth.assert_eq(4, boxes_needed)
}

test "string_operations_edge_cases" {
  // 测试字符串操作的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, ABCdef123!", @azimuth.greet("ABCdef123"))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：计算项目总工时和所需人员数量
  let tasks = 5
  let hours_per_task = 8
  let total_hours = @azimuth.multiply(tasks, hours_per_task)
  
  let working_hours_per_day = 8
  let days_needed = @azimuth.divide_with_ceil(total_hours, working_hours_per_day)
  
  let buffer_days = 2
  let total_days = @azimuth.add(days_needed, buffer_days)
  
  @azimuth.assert_eq(7, total_days)
}

test "mathematical_identities_verification" {
  // 测试数学恒等式验证
  let x = 42
  let y = 17
  
  // 加法恒等式
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  
  // 乘法恒等式
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  
  // 分配律验证
  let z = 3
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
}