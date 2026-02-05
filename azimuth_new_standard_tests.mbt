// Azimuth 项目新增的 MoonBit 测试用例
// 包含 8 个高质量的标准测试用例，测试核心功能

test "azimuth_core_arithmetic_precision" {
  // 测试算术运算的精度
  assert_eq(1000001, add(1000000, 1))
  assert_eq(999999, subtract(1000000, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "azimuth_core_negative_operations" {
  // 测试负数操作的全面性
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, subtract(-8, -3))
  assert_eq(21, multiply(-3, -7))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "azimuth_core_zero_handling" {
  // 测试零的处理
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(999, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "azimuth_core_business_calculation" {
  // 业务计算：订单总价
  let unit_price = 89
  let quantity = 6
  let shipping = 25
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(534, subtotal)
  assert_eq(43, tax)
  assert_eq(602, total)
}

test "azimuth_core_time_management" {
  // 时间管理：项目规划
  let total_tasks = 47
  let tasks_per_day = 8
  let work_days = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(work_days, 8)
  
  assert_eq(6, work_days)
  assert_eq(48, total_hours)
}

test "azimuth_core_resource_allocation" {
  // 资源分配：库存管理
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = subtract(total_capacity, total_products)
  
  assert_eq(7, boxes_needed)
  assert_eq(168, total_capacity)
  assert_eq(10, unused_space)
}

test "azimuth_core_string_processing" {
  // 字符串处理：国际化问候
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "azimuth_core_complex_workflow" {
  // 复杂工作流：多步骤计算
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let adjustment = 12
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, adjustment)
  
  assert_eq(60, step1)
  assert_eq(9, step2)
  assert_eq(21, result)
}