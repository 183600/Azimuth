// Azimuth 标准测试用例 - 8个高质量测试
// 测试核心功能的各种实际应用场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(-2, subtract(5, 7))
  assert_eq(24, multiply(6, 4))
  assert_eq(0, multiply(0, 100))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0时返回0
}

test "greet_function_basic" {
  // 问候函数基本测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping_cost = 25
  let discount = 30
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(295, total_before_discount)
  assert_eq(265, final_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 58
  let items_per_box = 15
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_capacity = multiply(boxes_needed, items_per_box)
  let wasted_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)      // ceil(58/15) = 4
  assert_eq(60, total_capacity)  // 4 * 15 = 60
  assert_eq(2, wasted_space)     // 60 - 58 = 2
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 7
  let total_tasks = 32
  let work_hours_per_day = 8
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_work_hours = multiply(days_needed, work_hours_per_day)
  
  assert_eq(5, days_needed)       // ceil(32/7) = 5
  assert_eq(40, total_work_hours) // 5 * 8 = 40
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}