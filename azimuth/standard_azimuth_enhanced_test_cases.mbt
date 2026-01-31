// 标准 Azimuth 增强测试用例
// 包含10个高质量的 MoonBit 标准测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(8, 7))
  assert_eq(56, multiply(7, 8))
  assert_eq(1, subtract(9, 8))
  assert_eq(-1, subtract(8, 9))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(12, 3))
}

test "boundary_conditions_extreme" {
  // 极端边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 999999))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(10, add(-5, 15))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(2, divide_with_ceil(-7, -3))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 特殊字符!", greet("特殊字符"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "division_precision_scenarios" {
  // 除法精度场景测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(667, divide_with_ceil(2000, 3))
  assert_eq(334, divide_with_ceil(1000, 3))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：项目管理与资源分配
  let team_size = 8
  let project_duration = 12  // 周
  let tasks_per_person_per_week = 3
  let total_tasks = 250
  
  // 计算团队总产能
  let weekly_capacity = multiply(team_size, tasks_per_person_per_week)
  let total_capacity = multiply(weekly_capacity, project_duration)
  
  // 计算需要的额外时间
  let additional_weeks = divide_with_ceil(
    subtract(total_tasks, total_capacity),
    weekly_capacity
  )
  
  assert_eq(24, weekly_capacity)
  assert_eq(288, total_capacity)
  assert_eq(1, additional_weeks)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 结合律
  assert_eq(
    add(add(x, y), 5),
    add(x, add(y, 5))
  )
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 各种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  // 场景：多产品仓库存储优化
  let product_a_qty = 156
  let product_b_qty = 89
  let product_c_qty = 234
  let box_capacity = 25
  
  // 计算各产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  let total_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  
  // 验证容量充足
  assert_true(total_capacity >= total_items)
  
  // 计算空间利用率
  let utilization = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(93, utilization)
}

test "performance_large_scale_computations" {
  // 大规模计算性能测试
  let base_value = 1000
  let iterations = 100
  
  // 批量计算测试
  let sum_result = multiply(base_value, iterations)
  let product_result = multiply(base_value, base_value)
  let division_result = divide_with_ceil(product_result, base_value)
  
  assert_eq(100000, sum_result)
  assert_eq(1000000, product_result)
  assert_eq(1000, division_result)
  
  // 复杂数列计算
  let series_sum = add(
    multiply(base_value, 1),
    add(multiply(base_value, 2), multiply(base_value, 3))
  )
  let series_average = divide_with_ceil(series_sum, 3)
  
  assert_eq(6000000, series_sum)
  assert_eq(2000000, series_average)
}