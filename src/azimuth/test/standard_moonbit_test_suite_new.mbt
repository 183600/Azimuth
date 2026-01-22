// 新增的 MoonBit 标准测试用例
// 针对 azimuth 库的核心功能进行测试

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(21, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-5, 7))
  @azimuth.assert_eq(16, @azimuth.multiply(-4, -4))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

test "greet_function_various_inputs" {
  // 问候函数的各种输入测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, A+B=C!", @azimuth.greet("A+B=C"))
}

test "mathematical_properties_verification" {
  // 数学性质验证
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  let left = @azimuth.multiply(x, @azimuth.add(y, z))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left, right)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算
  let apple_price = 5
  let apple_qty = 6
  let banana_price = 3
  let banana_qty = 8
  let shipping = 10
  
  let apple_total = @azimuth.multiply(apple_price, apple_qty)
  let banana_total = @azimuth.multiply(banana_price, banana_qty)
  let subtotal = @azimuth.add(apple_total, banana_total)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(30, apple_total)
  @azimuth.assert_eq(24, banana_total)
  @azimuth.assert_eq(54, subtotal)
  @azimuth.assert_eq(64, total)
}

test "packaging_and_shipping" {
  // 包装和运输计算
  let total_items = 47
  let items_per_box = 12
  let cost_per_box = 8
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  let total_cost = @azimuth.multiply(boxes_needed, cost_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  @azimuth.assert_eq(32, total_cost)  // 4 * 8 = 32
}

test "project_timeline_calculation" {
  // 项目时间线计算
  let total_tasks = 85
  let tasks_per_day = 7
  let work_hours_per_day = 8
  
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = @azimuth.multiply(days_needed, work_hours_per_day)
  
  @azimuth.assert_eq(13, days_needed)  // ceil(85/7) = 13
  @azimuth.assert_eq(104, total_hours)  // 13 * 8 = 104
}

test "financial_calculations" {
  // 金融计算场景
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  // 简单的年复利计算
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, interest_rate), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, interest_rate), 100))
  
  @azimuth.assert_eq(1050, year1)
  @azimuth.assert_eq(1103, year2)
  @azimuth.assert_eq(1158, year3)
}

test "large_number_operations" {
  // 大数运算测试
  let large_num1 = 100000
  let large_num2 = 200000
  let large_num3 = 500
  
  let sum = @azimuth.add(large_num1, large_num2)
  let product = @azimuth.multiply(large_num3, 1000)
  let division = @azimuth.divide_with_ceil(sum, large_num3)
  
  @azimuth.assert_eq(300000, sum)
  @azimuth.assert_eq(500000, product)
  @azimuth.assert_eq(600, division)  // ceil(300000/500) = 600
}