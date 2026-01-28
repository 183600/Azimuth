// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试覆盖

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-8, 13))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "greeting_functionality" {
  // 问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 正数向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 负数向上取整除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 8
  let y = 12
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_optimization" {
  // 包装优化测试
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_capacity = multiply(boxes_needed, items_per_box)
  let wasted_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, wasted_space)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_budget = 1000
  let team_members = 6
  let min_budget_per_member = divide_with_ceil(total_budget, team_members)
  
  assert_eq(167, min_budget_per_member)
  
  let total_allocated = multiply(min_budget_per_member, team_members)
  assert_eq(1002, total_allocated)
}

test "time_management_calculation" {
  // 时间管理计算测试
  let tasks_per_day = 8
  let total_tasks = 35
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(5, days_needed)
  assert_eq(40, total_hours)
}