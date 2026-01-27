// 标准 MoonBit 测试用例 - 新增10个测试
// 覆盖 azimuth 库的核心功能和边界情况

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 25), @azimuth.add(25, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

test "multiply_zero_property" {
  // 测试乘法零元素性质：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(50, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-25, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))  // 完全相等
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))  // 99.99 -> 100
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 50))  // 零除法
}

test "greet_function_special_inputs" {
  // 测试问候函数的特殊输入
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))  // 数字字符串
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))  // Emoji
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let product_price = 120
  let quantity = 8
  let tax_rate = 8
  let shipping = 25
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(960, subtotal)
  @azimuth.assert_eq(77, tax)
  @azimuth.assert_eq(1062, total)
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-15, @azimuth.add(-8, -7))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "large_number_calculations" {
  // 大数计算测试
  let big_num = 10000
  @azimuth.assert_eq(20000, @azimuth.add(big_num, big_num))
  @azimuth.assert_eq(100000000, @azimuth.multiply(big_num, big_num))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(big_num, 10001))
  @azimuth.assert_eq(10001, @azimuth.divide_with_ceil(10001000, big_num))
}

test "resource_allocation_scenario" {
  // 资源分配场景
  let total_budget = 5000
  let project_costs = @azimuth.multiply(750, 4)
  let remaining_budget = @azimuth.add(total_budget, -project_costs)
  let small_projects = @azimuth.divide_with_ceil(remaining_budget, 200)
  
  @azimuth.assert_eq(3000, project_costs)
  @azimuth.assert_eq(2000, remaining_budget)
  @azimuth.assert_eq(10, small_projects)
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 6
  let total_tasks = 47
  let work_days = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = @azimuth.multiply(work_days, 8)  // 每天8小时
  
  @azimuth.assert_eq(8, work_days)  // ceil(47/6) = 8
  @azimuth.assert_eq(64, total_hours)  // 8 * 8 = 64
}

test "mathematical_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 4 * (7 + 9) = 4 * 7 + 4 * 9 = 64
}