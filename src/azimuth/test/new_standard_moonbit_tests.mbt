// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试
// 包含边界情况、数学性质和实际应用场景

test "add_comprehensive_test" {
  // 全面的加法测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-4, @azimuth.add(-7, 3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(-1, @azimuth.add(2147483647, -2147483648))
}

test "multiply_comprehensive_test" {
  // 全面的乘法测试
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-21, @azimuth.multiply(-7, 3))
  @azimuth.assert_eq(49, @azimuth.multiply(-7, -7))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_comprehensive_test" {
  // 全面的问候测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
}

test "divide_with_ceil_comprehensive_test" {
  // 全面的向上取整除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
}

test "mathematical_laws_test" {
  // 数学定律测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "business_calculation_test" {
  // 业务计算测试
  let unit_price = 45
  let quantity = 12
  let tax_rate = 10
  let shipping = 15
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(540, subtotal)
  @azimuth.assert_eq(54, tax)
  @azimuth.assert_eq(609, total)
}

test "packaging_optimization_test" {
  // 包装优化测试
  let items_per_box = 24
  let total_items_small = 50
  let total_items_medium = 150
  let total_items_large = 500
  
  let boxes_small = @azimuth.divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = @azimuth.divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = @azimuth.divide_with_ceil(total_items_large, items_per_box)
  
  let total_boxes = @azimuth.add(@azimuth.add(boxes_small, boxes_medium), boxes_large)
  
  @azimuth.assert_eq(3, boxes_small)
  @azimuth.assert_eq(7, boxes_medium)
  @azimuth.assert_eq(21, boxes_large)
  @azimuth.assert_eq(31, total_boxes)
}

test "time_management_test" {
  // 时间管理测试
  let tasks_per_day = 6
  let total_tasks = 47
  let hours_per_task = 2
  
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = @azimuth.multiply(days_needed, tasks_per_day * hours_per_task)
  
  @azimuth.assert_eq(8, days_needed)
  @azimuth.assert_eq(96, total_hours)
}

test "complex_expression_test" {
  // 复杂表达式测试
  let x = 15
  let y = 25
  let z = 10
  
  // 计算: (x + y) * z - x / y
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  let result2 = @azimuth.divide_with_ceil(x, y)
  let final_result = @azimuth.add(result1, -result2)
  
  @azimuth.assert_eq(400, result1)
  @azimuth.assert_eq(1, result2)
  @azimuth.assert_eq(399, final_result)
}

test "error_handling_test" {
  // 错误处理测试
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试零的运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
}