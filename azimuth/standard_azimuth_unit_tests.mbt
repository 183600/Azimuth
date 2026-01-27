// 标准 Azimuth 单元测试
// 使用标准 MoonBit 测试语法编写的高质量测试用例

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-5, 2))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_basic_operations" {
  // 测试减法函数的基本运算
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(2, 5))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, 2)
  
  assert_eq(12, result)  // ceil(10*3/4) + 2 = 10
}

test "error_handling_division_by_zero" {
  // 错误处理：除零测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}