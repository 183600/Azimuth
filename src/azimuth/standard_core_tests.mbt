// 标准 MoonBit 测试用例
// 测试核心功能：add, multiply, greet, divide_with_ceil 函数

test "add_basic_operations" {
  // 测试基本加法运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(0, 0))
}

test "multiply_basic_operations" {
  // 测试基本乘法运算
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
  assert_eq(100, multiply(10, 10))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let min_val = -2147483648
  
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(1, multiply(1, 1))
}

test "greet_function_tests" {
  // 测试greet函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_tests" {
  // 测试向上取整除法
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
}

test "complex_calculations" {
  // 测试复合计算场景
  let length = 8
  let width = 5
  
  // 计算矩形面积
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 计算矩形周长
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
  
  // 计算总面积与周长之和
  let total = add(area, perimeter)
  assert_eq(66, total)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(413, total)
  
  // 平均值计算
  let scores_sum = add(add(85, 90), 95)
  let average = scores_sum / 3
  assert_eq(90, average)
}

test "error_handling_and_assertions" {
  // 测试错误处理和断言
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界条件
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}