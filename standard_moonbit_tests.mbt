// 标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

test "add_function_basic" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_function_basic" {
  // 测试乘法函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
}

test "greet_function_basic" {
  // 测试问候函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(1000, add(500, 500))
  assert_eq(10, add(15, -5))
  assert_eq(-10, add(-15, 5))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
  assert_eq(1, multiply(1, 1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_calculation" {
  // 测试业务计算场景
  let unit_price = 45
  let quantity = 6
  let subtotal = multiply(unit_price, quantity)
  
  assert_eq(270, subtotal)
}

test "packaging_optimization" {
  // 测试包装优化计算
  let total_items = 58
  let box_capacity = 12
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let wasted_space = subtract(total_capacity, total_items)
  
  assert_eq(5, boxes_needed)
  assert_eq(60, total_capacity)
  assert_eq(2, wasted_space)
}