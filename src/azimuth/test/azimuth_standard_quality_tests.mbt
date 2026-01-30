// Azimuth 项目标准质量测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(5, @azimuth.add(-2, 7))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(34, @azimuth.divide_with_ceil(100, 3))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-33333, @azimuth.divide_with_ceil(-100000, 3))
}

test "combined_operations_business_scenario" {
  // 组合运算业务场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(215, total_before_discount)
  @azimuth.assert_eq(195, final_total)
}

test "packaging_calculation_scenario" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 测试多个包装需求
  let small_items = 25
  let large_items = 100
  let small_boxes = @azimuth.divide_with_ceil(small_items, items_per_box)
  let large_boxes = @azimuth.divide_with_ceil(large_items, items_per_box)
  let total_boxes = @azimuth.add(small_boxes, large_boxes)
  
  @azimuth.assert_eq(3, small_boxes)
  @azimuth.assert_eq(9, large_boxes)
  @azimuth.assert_eq(12, total_boxes)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  
  // 分配律验证
  let c = 5
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "complex_real_world_calculation" {
  // 复杂真实世界计算：项目管理
  let team_members = 8
  let tasks_per_member = 5
  let days_per_task = 3
  let contingency_days = 7
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  // 计算基础天数
  let base_days = @azimuth.multiply(total_tasks, days_per_task)
  
  // 计算总项目天数（包含应急时间）
  let total_days = @azimuth.add(base_days, contingency_days)
  
  // 计算需要的周数（工作周，每周5天）
  let weeks_needed = @azimuth.divide_with_ceil(total_days, 5)
  
  @azimuth.assert_eq(40, total_tasks)
  @azimuth.assert_eq(120, base_days)
  @azimuth.assert_eq(127, total_days)
  @azimuth.assert_eq(26, weeks_needed)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 除数为零的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  let large_number = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(large_number, large_number))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 边界除法测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
}