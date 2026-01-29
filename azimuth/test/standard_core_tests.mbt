// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含 8 个高质量测试用例，覆盖主要功能和边界情况

test "add_function_basic_operations" {
  // 测试加法函数的基本操作
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
}

test "multiply_function_basic_operations" {
  // 测试乘法函数的基本操作
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "business_logic_packaging" {
  // 测试业务逻辑：包装计算
  let items = 25
  let capacity_per_box = 6
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(30, total_capacity)
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let base_value = 10
  let step1 = add(base_value, 5)        // 15
  let step2 = multiply(step1, 2)        // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = add(step3, 1)             // 5
  
  assert_eq(5, step4)
}