// 用户增强测试用例 - 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的额外测试覆盖

test "subtract_comprehensive_test" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(1000000, subtract(1000001, 1))
}

test "mathematical_commutative_laws_extended" {
  // 扩展的数学交换律测试
  let a = 42
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证交换律在负数情况下也成立
  assert_eq(add(-a, b), add(b, -a))
  assert_eq(multiply(-a, b), multiply(b, -a))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(667, divide_with_ceil(2000, 3))
  assert_eq(334, divide_with_ceil(1000, 3))
  assert_eq(1, divide_with_ceil(-1, -2))
  assert_eq(-1, divide_with_ceil(1, -2))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边缘情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello, moon!", greet("moon"))
  assert_eq_string("Hello, test!", greet("test"))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)     // 3
  let step5 = add(step4, multiply(2, 3))  // 9
  
  assert_eq(9, step5)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let teams = 3
  let min_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_per_team)  // ceil(100/3) = 34
  
  // 验证总分配
  let total_allocated = multiply(min_per_team, teams)
  assert_eq(102, total_allocated)
  
  // 验证剩余资源
  let remaining = subtract(total_allocated, total_resources)
  assert_eq(2, remaining)
}

test "error_handling_comprehensive" {
  // 全面错误处理测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证在负数情况下也成立
  let left_side_neg = multiply(-a, add(b, c))
  let right_side_neg = add(multiply(-a, b), multiply(-a, c))
  assert_eq(left_side_neg, right_side_neg)
}

test "string_processing_simple" {
  // 简单字符串处理测试
  assert_eq_string("Hello, quoted!", greet("quoted"))
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, backslash!", greet("backslash"))
  assert_eq_string("Hello, brackets!", greet("brackets"))
  assert_eq_string("Hello, braces!", greet("braces"))
}

test "simple_inventory_calculation" {
  // 简单库存计算测试
  let product_a = 25
  let box_capacity_a = 8
  let boxes_a = divide_with_ceil(product_a, box_capacity_a)  // ceil(25/8) = 4
  
  let product_b = 47
  let box_capacity_b = 12
  let boxes_b = divide_with_ceil(product_b, box_capacity_b)  // ceil(47/12) = 4
  
  // 计算总箱子数
  let total_boxes = add(boxes_a, boxes_b)
  assert_eq(8, total_boxes)
  
  // 计算总存储容量
  let capacity_a = multiply(boxes_a, box_capacity_a)  // 4 * 8 = 32
  let capacity_b = multiply(boxes_b, box_capacity_b)  // 4 * 12 = 48
  
  let total_capacity = add(capacity_a, capacity_b)
  assert_eq(80, total_capacity)
  
  // 计算剩余空间
  let total_products = add(product_a, product_b)
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(8, remaining_space)
}