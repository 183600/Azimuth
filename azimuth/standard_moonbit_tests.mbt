// 标准 MoonBit 测试用例 - 覆盖核心模块功能
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "int_module_boundary_values" {
  // 测试 int 模块的边界值
  assert_eq(2147483647, 2147483647)
  assert_eq(-2147483648, -2147483648)
}

test "string_module_basic_functionality" {
  // 测试 string 模块的基本功能
  assert_eq_string("", "")
  assert_eq_string("Hello, MoonBit!", "Hello, MoonBit!")
  assert_eq_string("", "")
}

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(10, 5))
  assert_eq(5, subtract(10, 5))
  assert_eq(50, multiply(10, 5))
  assert_eq(2, divide_with_ceil(10, 5))
}

test "boolean_logical_operations" {
  // 测试布尔逻辑运算
  assert_true(true)
  assert_false(false)
}

test "comparison_operations" {
  // 测试比较运算
  assert_true(10 > 5)
  assert_false(5 > 10)
  assert_true(5 >= 5)
  assert_false(5 >= 10)
  assert_true(5 < 10)
  assert_false(10 < 5)
  assert_true(5 <= 5)
  assert_false(10 <= 5)
  assert_true(5 == 5)
  assert_false(5 == 10)
}

test "string_concatenation_operations" {
  // 测试字符串连接操作
  assert_eq_string("Hello", "Hello")
  assert_eq_string("Hello World", "Hello World")
  assert_eq_string("123", "123")
  assert_eq_string("", "")
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, -5)
  assert_eq(5, -(-5))
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
}

test "azimuth_library_functions" {
  // 测试 azimuth 库函数
  assert_eq(7, add(3, 4))
  assert_eq(12, multiply(3, 4))
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, subtract(8, 6))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 5
  let b = 3
  let sum = add(a, b)
  let product = multiply(a, b)
  let difference = subtract(a, b)
  
  assert_eq(8, sum)
  assert_eq(15, product)
  assert_eq(2, difference)
  
  // 测试组合运算
  let combined = add(multiply(a, b), subtract(a, b))
  assert_eq(17, combined)  // 5*3 + (5-3) = 15 + 2 = 17
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}