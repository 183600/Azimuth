// Azimuth 标准单元测试 - 10个高质量的 MoonBit 测试用例
// 测试覆盖基本功能、边界条件和实际应用场景

test "add_function_positive_numbers" {
  // 测试加法函数的正数情况
  assert_eq(10, add(7, 3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
}

test "multiply_function_zero_handling" {
  // 测试乘法函数的零处理
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(5, multiply(5, 1))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, élan!", greet("élan"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "subtract_function_negative_results" {
  // 测试减法函数的负数结果
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(5, subtract(10, 5))
  assert_eq(-10, subtract(0, 10))
}

test "mathematical_commutative_property" {
  // 测试数学交换律性质
  let a = 15
  let b = 25
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_calculation" {
  // 测试业务包装计算场景
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let initial = 8
  let step1 = multiply(initial, 3)  // 24
  let step2 = add(step1, 10)        // 34
  let step3 = divide_with_ceil(step2, 5)  // 7
  let step4 = subtract(step3, 2)    // 5
  assert_eq(5, step4)
}

test "error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "real_world_event_planning" {
  // 测试真实世界活动规划场景
  let guests = 87
  let tables = 10
  let chairs_per_table = 8
  
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(11, tables_needed)
  
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_chairs)
  
  let available_tables = tables
  assert_true(tables_needed <= available_tables)
}