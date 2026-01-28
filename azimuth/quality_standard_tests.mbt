// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含 8 个高质量测试用例，覆盖核心功能和边界情况

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(1000, add(999, 1))
}

test "multiply_function_zero_cases" {
  // 测试乘法的零情况
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_function_basic_cases" {
  // 测试减法函数的基本情况
  assert_eq(5, subtract(10, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(5, 10))
}

test "mathematical_commutative_property" {
  // 测试数学交换律性质
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_error_handling" {
  // 测试向上取整除法的错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
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