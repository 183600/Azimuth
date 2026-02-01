// 新增的高质量 MoonBit 测试用例
// 覆盖边界条件、数学性质和实际应用场景

test "mathematical_commutative_law" {
  // 验证加法和乘法交换律
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 使用负数验证
  let c = -15
  let d = 8
  assert_eq(add(c, d), add(d, c))
  assert_eq(multiply(c, d), multiply(d, c))
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边界情况
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1001, divide_with_ceil(2001, 2))
  assert_eq(666667, divide_with_ceil(2000000, 3))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "string_greeting_multilingual_support" {
  // 测试多语言问候支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
}

test "complex_business_inventory_calculation" {
  // 复杂业务库存计算场景
  let product_a = 45
  let product_b = 67
  let product_c = 23
  let box_capacity = 20
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(9, total_boxes)  // ceil(45/20) + ceil(67/20) + ceil(23/20) = 3 + 4 + 2 = 9
  
  // 计算总存储容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(180, total_capacity)
  
  // 计算剩余空间
  let total_products = add(add(product_a, product_b), product_c)
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(45, remaining_space)
}

test "mathematical_distributive_property" {
  // 验证数学分配律
  let a = 7
  let b = 12
  let c = 5
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 使用负数验证
  let x = -8
  let y = 15
  let z = -3
  let left_neg = multiply(x, add(y, z))
  let right_neg = add(multiply(x, y), multiply(x, z))
  assert_eq(left_neg, right_neg)
}

test "error_handling_division_by_zero_comprehensive" {
  // 全面的除零错误处理测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "arithmetic_sequence_operations" {
  // 等差数列运算测试
  let first_term = 3
  let common_diff = 4
  let n_terms = 6
  
  // 计算第n项：a_n = a_1 + (n-1)*d
  let nth_term = add(first_term, multiply(subtract(n_terms, 1), common_diff))
  assert_eq(23, nth_term)  // 3 + (6-1)*4 = 23
  
  // 计算前n项和：S_n = n/2 * (a_1 + a_n)
  let sum_n = multiply(divide_with_ceil(n_terms, 2), add(first_term, nth_term))
  assert_eq(78, sum_n)  // 6/2 * (3 + 23) = 78
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 15000
  let departments = 4
  let min_budget_per_dept = 3000
  
  // 计算每个部门的基础预算
  let base_budget = divide_with_ceil(total_budget, departments)
  assert_eq(3750, base_budget)  // ceil(15000/4) = 3750
  
  // 验证所有部门都能满足最低预算要求
  assert_true(base_budget >= min_budget_per_dept)
  
  // 计算总分配预算
  let total_allocated = multiply(base_budget, departments)
  assert_eq(15000, total_allocated)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, multiply(min_budget_per_dept, departments))
  assert_eq(3000, remaining_budget)
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 15
  
  // 计算步骤：((value * 2 + 10) / 5) * 3 - 8
  let step1 = multiply(initial_value, 2)
  let step2 = add(step1, 10)
  let step3 = divide_with_ceil(step2, 5)
  let step4 = multiply(step3, 3)
  let final_result = subtract(step4, 8)
  
  // 手动计算验证：((15*2 + 10) / 5) * 3 - 8 = (40/5) * 3 - 8 = 8 * 3 - 8 = 24 - 8 = 16
  assert_eq(16, final_result)
}