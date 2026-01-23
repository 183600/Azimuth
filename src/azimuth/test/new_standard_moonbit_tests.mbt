// 新增的 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(1000, @azimuth.add(1000, 0))
  @azimuth.assert_eq(-1000, @azimuth.add(-1000, 0))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
  @azimuth.assert_eq(0, @azimuth.add(-1, 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(1000, @azimuth.multiply(1, 1000))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1000))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "greet_function_unicode" {
  // 测试问候函数的Unicode字符处理
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 10
  let y = 20
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result = @azimuth.add(@azimuth.multiply(@azimuth.add(x, y), z), -@azimuth.divide_with_ceil(x, y))
  @azimuth.assert_eq(149, result)  // (10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
}

test "real_world_shopping_scenario" {
  // 测试实际购物场景
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let total = @azimuth.add(@azimuth.multiply(item1_price, item1_qty), @azimuth.multiply(item2_price, item2_qty))
  @azimuth.assert_eq(103, total)  // 29*2 + 15*3 = 58 + 45 = 103
}

test "packaging_calculation" {
  // 测试包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "time_calculation_scenario" {
  // 测试时间计算场景
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(days_needed, 8)
  
  @azimuth.assert_eq(6, days_needed)  // ceil(45/8) = 6
  @azimuth.assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "error_handling_robustness" {
  // 测试错误处理健壮性
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(big_num, big_num))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证分配律：a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)  // 12 * (8 + 5) = 12 * 8 + 12 * 5 = 156
}