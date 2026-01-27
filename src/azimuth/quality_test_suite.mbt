// 高质量测试套件 - 包含 10 个精心设计的测试用例
// 覆盖 azimuth 库的核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-15, multiply(-3, 5))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "string_processing_capabilities" {
  // 字符串处理能力测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = divide_with_ceil(add(multiply(3, 4), multiply(5, 2)), 3)
  assert_eq(8, result3)  // ceil((12 + 10) / 3) = ceil(22/3) = 8
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 场景1: 计算购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
  
  // 场景2: 计算需要的包装箱数量
  let total_items = 28
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(28/6) = 5
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

test "large_number_computations" {
  // 大数计算测试
  let big_num = 100000
  assert_eq(200000, add(big_num, big_num))
  assert_eq(10000000000, multiply(big_num, big_num))
  assert_eq(33334, divide_with_ceil(10000000, 300))
  assert_eq(1, divide_with_ceil(99999, 100000))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 5
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "performance_efficiency_tests" {
  // 性能效率测试
  // 测试连续运算的效率
  let base = 10
  let iterations = 100
  
  // 模拟循环累加: base + base*2 + base*3 + ... + base*iterations
  let accumulated = multiply(base, multiply(iterations, add(iterations, 1)) / 2)
  assert_eq(50500, accumulated)  // 10 * (100 * 101 / 2) = 10 * 5050 = 50500
  
  // 测试除法精度
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(101, divide_with_ceil(1000, 10))
  assert_eq(1, divide_with_ceil(99, 100))
}