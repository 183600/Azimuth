// 标准 MoonBit 测试套件
// 为 azimuth 库提供全面的标准测试用例

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_comprehensive" {
  // 测试问候函数的各种情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_identity_elements" {
  // 测试数学单位元性质
  let test_value = 15
  
  // 加法单位元（0）
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  
  // 乘法单位元（1）
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, 1))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_int = 2147483647
  let small_number = 1
  
  // 边界值加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int - small_number, small_number))
  
  // 边界值乘法（与零）
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  
  // 边界值除法
  @azimuth.assert_eq(max_int, @azimuth.divide_with_ceil(max_int, 1))
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 大数运算
  let large_num = 100000
  @azimuth.assert_eq(200000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(10000000000, @azimuth.multiply(large_num, large_num))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)
  
  // 场景2：时间计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  @azimuth.assert_eq(5, days_needed)
  
  // 场景3：组合计算
  let result = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(5, 2))
  @azimuth.assert_eq(22, result)  // 3*4 + 5*2 = 12 + 10 = 22
}