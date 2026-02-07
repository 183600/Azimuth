// Azimuth 综合标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  assert_eq(10, add(4, 6))
  assert_eq(24, multiply(4, 6))
  assert_eq(-2, add(3, -5))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(20, multiply(-4, -5))
}

test "comprehensive_divide_ceil_functionality" {
  // 向上取整除法功能综合测试
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(3, divide_with_ceil(14, 5))
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(0, divide_with_ceil(0, 8))
  assert_eq(0, divide_with_ceil(7, 0))
}

test "comprehensive_negative_scenarios" {
  // 负数场景综合测试
  assert_eq(-5, divide_with_ceil(-20, 4))
  assert_eq(-4, divide_with_ceil(15, -4))
  assert_eq(4, divide_with_ceil(-15, -4))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(1, divide_with_ceil(-3, -3))
}

test "comprehensive_string_greeting" {
  // 问候字符串综合测试
  assert_eq_string("Hello, Developer!", greet("Developer"))
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "comprehensive_subtract_operations" {
  // 减法运算综合测试
  assert_eq(8, subtract(15, 7))
  assert_eq(-8, subtract(7, 15))
  assert_eq(0, subtract(12, 12))
  assert_eq(25, subtract(10, -15))
  assert_eq(-25, subtract(-10, 15))
}

test "comprehensive_mathematical_properties" {
  // 数学性质综合验证测试
  let x = 9
  let y = 6
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(15, add(9, 6))
  assert_eq(54, multiply(9, 6))
}

test "comprehensive_warehouse_calculation" {
  // 仓库计算场景测试
  let total_products = 87
  let boxes_per_pallet = 12
  let pallets_needed = divide_with_ceil(total_products, boxes_per_pallet)
  
  assert_eq(8, pallets_needed)
  
  // 验证总容量
  let total_capacity = multiply(pallets_needed, boxes_per_pallet)
  assert_eq(96, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(9, remaining_space)
}

test "comprehensive_project_planning" {
  // 项目规划复杂计算测试
  // 场景：计算项目资源需求
  let developers = 5
  let tasks_per_developer = 7
  let total_tasks = multiply(developers, tasks_per_developer)
  
  let weekly_capacity = 8
  let weeks_needed = divide_with_ceil(total_tasks, weekly_capacity)
  
  assert_eq(35, total_tasks)  // 5 * 7 = 35
  assert_eq(5, weeks_needed)  // ceil(35/8) = 5
  
  // 验证总周容量
  let total_weekly_capacity = multiply(weeks_needed, weekly_capacity)
  assert_eq(40, total_weekly_capacity)
  
  // 验证剩余容量
  let spare_capacity = subtract(total_weekly_capacity, total_tasks)
  assert_eq(5, spare_capacity)
}