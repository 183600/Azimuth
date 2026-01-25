// 标准 MoonBit 测试用例 - 为 clean_test 包添加的高质量测试
// 包含基本功能测试、边界条件测试和实际应用场景测试

test "add_function_comprehensive" {
  // 全面测试加法函数的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(100, add(50, 50))
}

test "multiply_function_various_scenarios" {
  // 测试乘法函数的各种场景
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 123))
  assert_eq(-15, multiply(3, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, empty!", greet("empty"))
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  assert_eq(
    add(a, b),
    add(b, a)
  )
  
  // 乘法交换律: a * b = b * a
  assert_eq(
    multiply(a, b),
    multiply(b, a)
  )
}

test "complex_business_calculation" {
  // 复杂业务计算场景：项目预算分配
  let total_budget = 50000
  let team_members = 8
  let base_salary = 4000
  
  // 计算基础薪资总额
  let total_base_salary = multiply(base_salary, team_members)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -total_base_salary)
  
  // 验证计算结果
  assert_eq(32000, total_base_salary)  // 4000 * 8 = 32000
  assert_eq(18000, remaining_budget)   // 50000 - 32000 = 18000
}

test "inventory_management_optimization" {
  // 库存管理优化场景
  let product_a_quantity = 156
  let product_b_quantity = 89
  let box_capacity = 24
  
  // 计算产品A需要的箱子数
  let mut boxes_a = product_a_quantity / box_capacity
  if product_a_quantity % box_capacity > 0 {
    boxes_a = boxes_a + 1
  }
  
  // 计算产品B需要的箱子数
  let mut boxes_b = product_b_quantity / box_capacity
  if product_b_quantity % box_capacity > 0 {
    boxes_b = boxes_b + 1
  }
  
  // 计算总箱子数
  let total_boxes = add(boxes_a, boxes_b)
  
  // 验证结果
  assert_eq(7, boxes_a)   // ceil(156/24) = 7
  assert_eq(4, boxes_b)   // ceil(89/24) = 4
  assert_eq(11, total_boxes)  // 7 + 4 = 11
}

test "mathematical_distributive_property" {
  // 验证数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(
    multiply(a, b),
    multiply(a, c)
  )
  
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "error_handling_edge_cases" {
  // 错误处理和边界情况测试
  // 测试极值运算
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(0, add(large_positive, large_negative))
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  
  // 测试零值乘法
  assert_eq(0, multiply(large_positive, 0))
  assert_eq(0, multiply(0, large_negative))
}

test "real_world_shipping_calculation" {
  // 真实世界运输计算场景
  let package_weights = add(15, 8)  // 总重量：23kg
  let max_weight_per_box = 10  // 每箱最大重量：10kg
  
  // 计算需要的箱子数量
  let mut boxes_needed = package_weights / max_weight_per_box
  if package_weights % max_weight_per_box > 0 {
    boxes_needed = boxes_needed + 1
  }
  
  // 计算总运力
  let total_capacity = multiply(boxes_needed, max_weight_per_box)
  
  // 验证结果
  assert_eq(3, boxes_needed)  // ceil(23/10) = 3
  assert_eq(30, total_capacity)  // 3 * 10 = 30
}

test "string_formatting_special_cases" {
  // 字符串格式化的特殊情况处理
  // 测试各种特殊字符串
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
}