// 标准 MoonBit 测试用例 - 覆盖 azimuth 库的核心功能
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_verification" {
  // 基本算术验证测试
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq(2, subtract(5, 3))
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_basic_operations" {
  // 问候函数基本操作测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_simple" {
  // 简单数学性质测试
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "calculation_scenario_packaging" {
  // 计算场景：包装问题
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-1, add(-2, 1))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "boundary_conditions_test" {
  // 边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let base_value = 10
  let step1 = multiply(base_value, 2)
  let step2 = divide_with_ceil(step1, 4)
  let result = add(step2, 5)
  
  assert_eq(10, result)  // (10*2)/4 + 5 = 5 + 5 = 10
}