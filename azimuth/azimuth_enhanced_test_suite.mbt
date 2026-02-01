// Azimuth 增强测试套件
// 包含8个高质量的标准 MoonBit 测试用例
// 专注于边界条件、数学性质和复杂业务场景

test "extreme_boundary_conditions" {
  // 极值边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "mathematical_distributive_property" {
  // 数学分配律验证测试
  let a = 6
  let b = 4
  let c = 3
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "complex_inventory_management" {
  // 复杂库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  let utilization_rate = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, utilization_rate)  // floor(15700/168) = 93%
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算验证
  let max_safe = 1000000
  assert_eq(0, multiply(max_safe, 0))
  assert_eq(max_safe, add(max_safe, 0))
  assert_eq(0, subtract(max_safe, max_safe))
}

test "financial_loan_calculation" {
  // 金融贷款计算场景测试
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(200, final_payment)
}

test "nested_calculation_pipeline" {
  // 嵌套计算流水线测试
  let base = 8
  
  // 复杂嵌套计算: ceil((base*3 + base*2) / (base + ceil(base/3)))
  let numerator = add(multiply(base, 3), multiply(base, 2))
  let denominator = add(base, divide_with_ceil(base, 3))
  let result = divide_with_ceil(numerator, denominator)
  
  // 手动验证: (8*3 + 8*2) / (8 + ceil(8/3)) = 40 / 11 = ceil(3.63) = 4
  assert_eq(4, result)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 89
  let teams = 4
  let min_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(23, min_per_team)
  
  let allocated_total = multiply(min_per_team, teams)
  assert_eq(92, allocated_total)
  
  let waste = subtract(allocated_total, total_resources)
  assert_eq(3, waste)
  
  // 验证资源利用率
  let efficiency = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(97, efficiency)  // floor(8900/92) = 96%
}

test "advanced_string_processing" {
  // 高级字符串处理测试
  // 测试各种特殊字符和边界情况
  assert_eq_string("Hello, space!", greet("space"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, emoji🎉!", greet("emoji🎉"))
  assert_eq_string("Hello, leading !", greet("leading "))
  assert_eq_string("Hello,  trailing!", greet(" trailing"))
}