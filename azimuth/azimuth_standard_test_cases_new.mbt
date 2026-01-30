// Azimuth 标准测试用例 - 新增10个高质量测试
// 使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_basic_cases" {
  // 问候函数基本测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_basic_cases" {
  // 减法函数基本测试
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(10, subtract(15, 5))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质验证
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_identity_elements" {
  // 数学单位元素测试
  let x = 42
  
  // 加法单位元素
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元素
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元素
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 场景：计算装盒问题
  let total_items = 47
  let items_per_box = 12
  
  // 计算需要的盒子数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  
  // 数值边界情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, subtract(5, 5))
}