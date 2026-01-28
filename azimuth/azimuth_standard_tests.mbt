// Azimuth 标准测试用例
// 使用标准 MoonBit 测试语法编写的测试用例

// 导入lib.mbt中定义的函数
// 这些函数已经在lib.mbt中定义

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(1000, add(999, 1))
}

test "add_function_with_zero" {
  // 测试与零的加法
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
}

test "multiply_function_basic" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(5, 0))
}

test "divide_with_ceil_basic_operations" {
  // 测试向上取整除法的基本操作
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_zero_divisor" {
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "greet_function_standard_inputs" {
  // 测试标准输入的问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "subtract_function_basic" {
  // 测试基本减法运算
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-2, subtract(3, 5))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 10
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算包装需求
  let items = 23
  let box_capacity = 5
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(25, total_capacity)
}

test "unicode_string_handling" {
  // 测试Unicode字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}