// 新增的标准 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(8, add(3, 5))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  assert_eq(-8, add(-3, -5))
  assert_eq(-2, add(3, -5))
  assert_eq(2, add(-3, 5))
}

test "multiply_function_various_cases" {
  // 测试乘法的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(3, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_different_inputs" {
  // 测试问候函数的不同输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_cases" {
  // 测试向上取整除法的负数情况
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-2, divide_with_ceil(10, -5))
  assert_eq(2, divide_with_ceil(-10, -5))
}

test "subtract_function_basic" {
  // 测试减法函数
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算装盒问题
  let items = 23
  let box_capacity = 5
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(25, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(2, remaining_space)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}