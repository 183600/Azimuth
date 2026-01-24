// 标准 Azimuth 核心测试用例
// 测试 src/azimuth/lib.mbt 中的核心函数

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(1000, add(1000, 0))
  assert_eq(-1000, add(-1000, 0))
  assert_eq(0, add(1, -1))
  assert_eq(0, add(-1, 1))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(1000, multiply(1, 1000))
  assert_eq(0, multiply(0, -1000))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000, divide_with_ceil(1000, 1))
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(2, divide_with_ceil(1001, 1000))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let add_result = add(multiply(3, 4), multiply(2, 5))
  assert_eq(22, add_result)  // 3*4 + 2*5 = 12 + 10 = 22
  
  let divide_result = divide_with_ceil(add_result, 7)
  assert_eq(4, divide_result)  // ceil(22/7) = 4
}

test "business_logic_calculation" {
  // 测试业务逻辑计算：装盒问题
  let total_items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}