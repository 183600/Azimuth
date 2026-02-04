// 新的标准 MoonBit 测试用例
// 包含8个高质量测试用例，覆盖不同的场景和边界情况

test "large_number_arithmetic" {
  // 大数算术测试
  assert_eq(100000000, add(50000000, 50000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(500000000, subtract(1000000000, 500000000))
  assert_eq(100000, divide_with_ceil(99999999, 1000))
}

test "mixed_sign_operations" {
  // 混合符号运算测试
  assert_eq(-5, add(10, -15))
  assert_eq(-30, multiply(6, -5))
  assert_eq(20, subtract(-10, -30))
  assert_eq(-2, divide_with_ceil(-20, 9))
}

test "unicode_greeting_extended" {
  // 扩展的 Unicode 问候测试
  assert_eq_string("Hello, 🌙✨!", greet("🌙✨"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "business_inventory_management" {
  // 库存管理业务测试
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  let unused_capacity = subtract(total_capacity, total_products)
  assert_eq(10, unused_capacity)
}

test "mathematical_distributive_law" {
  // 数学分配律测试
  let a = 8
  let b = 5
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 8*(5+3) = 8*5 + 8*3 = 64
}

test "event_planning_scenario" {
  // 活动规划场景测试
  let guests = 125
  let tables = 15
  let chairs_per_table = 8
  
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(16, tables_needed)
  
  let available_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(128, available_chairs)
  
  let extra_chairs = subtract(available_chairs, guests)
  assert_eq(3, extra_chairs)
  
  assert_true(tables_needed <= tables)
}

test "edge_case_zero_operations" {
  // 零操作边界情况测试
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, 0))
  
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
  assert_eq(0, subtract(0, 0))
  
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let offset = 3
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, offset)
  
  assert_eq(12, result)  // ceil(15*4/7) + 3 = 9 + 3 = 12
}