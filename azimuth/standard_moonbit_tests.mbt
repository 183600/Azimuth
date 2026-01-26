// 标准 MoonBit 测试用例 - 为 azimuth 库设计
// 包含9个高质量测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(56, multiply(7, 8))
  assert_eq(1, subtract(9, 8))
  assert_eq(3, divide_with_ceil(8, 3))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-3, add(-5, 2))
  assert_eq(10, multiply(-2, -5))
  assert_eq(-7, subtract(-2, 5))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "zero_element_properties" {
  // 零元素性质测试
  assert_eq(42, add(42, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "greeting_functionality" {
  // 问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 场景：计算包装需求
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "edge_case_boundary_values" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1000000, multiply(1000, 1000))
}

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)     // 3
  let final_result = add(step4, 2)   // 5
  
  assert_eq(5, final_result)
}