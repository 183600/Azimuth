// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

// 导入 azimuth 库
use azimuth

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, azimuth.add(7, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-5, azimuth.add(-2, -3))
  assert_eq(5, azimuth.add(-2, 7))
  assert_eq(1000, azimuth.add(500, 500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(21, azimuth.multiply(7, 3))
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(-6, azimuth.multiply(-2, 3))
  assert_eq(6, azimuth.multiply(-2, -3))
  assert_eq(100, azimuth.multiply(10, 10))
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))
  assert_eq(0, azimuth.divide_with_ceil(5, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 123!", azimuth.greet("123"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = azimuth.multiply(unit_price, quantity)
  let total_before_discount = azimuth.add(subtotal, shipping)
  let final_total = azimuth.add(total_before_discount, -discount)
  
  azimuth.assert_eq(200, subtotal)
  azimuth.assert_eq(215, total_before_discount)
  azimuth.assert_eq(195, final_total)
}

test "packaging_calculation_scenario" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  
  azimuth.assert_eq(4, boxes_needed)
}

test "mathematical_properties_verification" {
  // 数学性质验证
  let a = 7
  let b = 13
  
  // 加法交换律
  azimuth.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  azimuth.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 零元素性质
  azimuth.assert_eq(0, azimuth.multiply(a, 0))
  azimuth.assert_eq(a, azimuth.add(a, 0))
}

test "combined_operations_test" {
  // 组合运算测试
  let result1 = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = azimuth.multiply(azimuth.add(5, 3), 2)
  azimuth.assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "time_calculation_scenario" {
  // 时间计算场景
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  azimuth.assert_eq(5, days_needed)
  
  // 计算总工作小时数
  let total_hours = azimuth.multiply(days_needed, 8)
  azimuth.assert_eq(40, total_hours)
}