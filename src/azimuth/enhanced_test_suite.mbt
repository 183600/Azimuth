// 新增的高质量 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "addition_comprehensive_test" {
  // 全面的加法测试
  assert_eq(42, add(15, 27))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(100, -100))
}

test "multiplication_comprehensive_test" {
  // 全面的乘法测试
  assert_eq(96, multiply(12, 8))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法的精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greeting_unicode_test" {
  // 问候功能的 Unicode 测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 7
  let tax_rate = 8
  let shipping = 15
  let discount = 25
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算折扣前总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  // 计算最终总价
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(623, subtotal)
  assert_eq(50, tax_amount)
  assert_eq(688, total_before_discount)
  assert_eq(663, final_total)
}

test "mathematical_properties_test" {
  // 数学性质验证
  let x = 17
  let y = 23
  let z = 31
  
  // 验证加法交换律
  let add_comm1 = add(x, y)
  let add_comm2 = add(y, x)
  assert_eq(add_comm1, add_comm2)
  
  // 验证乘法交换律
  let mult_comm1 = multiply(x, y)
  let mult_comm2 = multiply(y, x)
  assert_eq(mult_comm1, mult_comm2)
  
  // 验证分配律：x * (y + z) = x * y + x * z
  let distrib_left = multiply(x, add(y, z))
  let distrib_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distrib_left, distrib_right)
}

test "resource_allocation_test" {
  // 资源分配优化问题
  let total_budget = 10000
  let project_cost1 = 1200
  let project_cost2 = 2300
  let project_cost3 = 1800
  let project_cost4 = 2700
  let project_cost5 = 1500
  
  // 计算项目总成本
  let total_cost = add(
    add(
      add(project_cost1, project_cost2),
      add(project_cost3, project_cost4)
    ),
    project_cost5
  )
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -total_cost)
  
  // 计算可以支持的小项目数量（每个500）
  let small_projects = divide_with_ceil(remaining_budget, 500)
  
  assert_eq(9500, total_cost)
  assert_eq(500, remaining_budget)
  assert_eq(1, small_projects)
}

test "time_calculation_test" {
  // 基于时间的计算：项目完成时间估算
  let total_tasks = 157
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(20, work_days_needed)
  assert_eq(4, weeks_needed)
  assert_eq(160, total_hours)
}

test "error_handling_test" {
  // 错误处理健壮性测试
  // 测试除零安全性
  let divide_by_zero_1 = divide_with_ceil(100, 0)
  let divide_by_zero_2 = divide_with_ceil(-100, 0)
  let divide_by_zero_3 = divide_with_ceil(0, 0)
  
  assert_eq(0, divide_by_zero_1)
  assert_eq(0, divide_by_zero_2)
  assert_eq(0, divide_by_zero_3)
  
  // 测试极端数值
  let extreme_positive = 2147483647
  let extreme_negative = -2147483647
  
  // 测试极端值的加法
  let extreme_add = add(extreme_positive, extreme_negative)
  assert_eq(0, extreme_add)
  
  // 测试极端值的乘法
  let extreme_mult = multiply(extreme_positive, 0)
  assert_eq(0, extreme_mult)
}

test "packaging_optimization_test" {
  // 包装优化问题
  let items_per_box = 12
  let total_items_small = 35
  let total_items_medium = 78
  let total_items_large = 156
  
  // 计算每种类型需要的箱子数
  let boxes_small = divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = divide_with_ceil(total_items_large, items_per_box)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_small, boxes_medium), boxes_large)
  
  // 计算总容量
  let total_capacity = multiply(total_boxes, items_per_box)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, add(add(total_items_small, total_items_medium), total_items_large))
  
  assert_eq(3, boxes_small)
  assert_eq(7, boxes_medium)
  assert_eq(13, boxes_large)
  assert_eq(23, total_boxes)
  assert_eq(276, total_capacity)
  assert_eq(7, remaining_space)
}